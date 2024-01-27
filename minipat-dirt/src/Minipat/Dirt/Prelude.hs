{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Prelude where

import Control.Applicative (empty)
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (SomeException, bracket, throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Dahdit.Midi.Osc (Datum (..), Packet)
import Dahdit.Network (Conn (..), HostPort (..), resolveAddr, runDecoder, runEncoder, udpServerConn)
import Data.Acquire (Acquire)
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Minipat.Base qualified as B
import Minipat.Dirt.Osc qualified as O
import Minipat.Dirt.Ref (Ref, RelVar)
import Minipat.Dirt.Ref qualified as R
import Minipat.Time qualified as T
import Nanotime (PosixTime (..), TimeDelta, TimeLike (..), threadDelayDelta, timeDeltaFromFracSecs)
import Network.Socket qualified as NS

data Env = Env
  { envTargetHp :: !HostPort
  , envListenHp :: !HostPort
  , envCps :: !Rational
  , envOscTimeout :: !TimeDelta
  }
  deriving stock (Eq, Ord, Show)

defaultEnv :: Env
defaultEnv =
  Env
    { envTargetHp = HostPort (Just "127.0.0.1") 57120
    , envListenHp = HostPort (Just "127.0.0.1") 57129
    , envCps = 1 % 2 -- 120 bpm, 4 bpc
    , envOscTimeout = timeDeltaFromFracSecs @Double 0.1
    }

data Domain = Domain
  { domCps :: !(TVar Rational)
  , domAhead :: !(TVar TimeDelta)
  , domPlaying :: !(TVar Bool)
  , domCycle :: !(TVar Integer)
  , domPat :: !(TVar (B.Pat O.OscMap))
  }

newDomain :: IO Domain
newDomain =
  Domain
    <$> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO False
    <*> newTVarIO 0
    <*> newTVarIO empty

initDomain :: Env -> IO Domain
initDomain env = newDomain >>= \d -> d <$ reinitDomain env d

reinitDomain :: Env -> Domain -> IO ()
reinitDomain env dom = atomically $ do
  let cps = envCps env
      td = timeDeltaFromFracSecs (1 / cps)
  writeTVar (domCps dom) cps
  writeTVar (domAhead dom) td
  writeTVar (domPlaying dom) False
  writeTVar (domCycle dom) 0
  writeTVar (domPat dom) empty

getCps :: St -> IO Rational
getCps = readTVarIO . domCps . stDom

getAhead :: St -> IO TimeDelta
getAhead = readTVarIO . domAhead . stDom

getPlaying :: St -> IO Bool
getPlaying = readTVarIO . domPlaying . stDom

getPat :: St -> IO (B.Pat O.OscMap)
getPat = readTVarIO . domPat . stDom

getCycle :: St -> IO Integer
getCycle = readTVarIO . domCycle . stDom

setCps :: St -> Rational -> IO ()
setCps st cps' = atomically $ do
  let dom = stDom st
  writeTVar (domCps dom) cps'
  writeTVar (domAhead dom) (timeDeltaFromFracSecs (1 / cps'))

setPlaying :: St -> Bool -> IO ()
setPlaying st x = atomically (writeTVar (domPlaying (stDom st)) x)

setPat :: St -> B.Pat O.OscMap -> IO ()
setPat st x = atomically (writeTVar (domPat (stDom st)) x)

setCycle :: St -> Integer -> IO ()
setCycle st x = atomically (writeTVar (domCycle (stDom st)) x)

advanceCycle :: Domain -> PosixTime -> STM O.PlayRecord
advanceCycle dom now = do
  cps <- readTVar (domCps dom)
  cyc <- fmap fromInteger (readTVar (domCycle dom))
  pat <- readTVar (domPat dom)
  let tape = B.unPat pat (T.Arc cyc (cyc + 1))
      dawn = addTime now (negate (timeDeltaFromFracSecs (cps * cyc)))
  modifyTVar' (domCycle dom) (+ 1)
  pure (O.PlayRecord dawn cps tape)

data OscConn = OscConn
  { ocTargetAddr :: !NS.SockAddr
  , ocListenConn :: !(Conn NS.SockAddr)
  }

data St = St
  { stEnv :: !Env
  , stRel :: !RelVar
  , stDom :: !Domain
  , stConn :: !(Ref OscConn)
  , stThd :: !(Ref (Async ()))
  }

acqConn :: Env -> Acquire OscConn
acqConn (Env targetHp listenHp _ _) = do
  targetAddr <- liftIO (resolveAddr targetHp)
  conn <- udpServerConn Nothing listenHp
  pure (OscConn targetAddr conn)

acqThd :: Ref OscConn -> Domain -> Acquire (Async ())
acqThd conn dom = R.acquireLoop (domAhead dom) (advanceThread conn dom)

initSt :: Env -> IO St
initSt env = do
  rv <- R.relVarInit
  dom <- initDomain env
  -- conn <- R.refEmpty
  -- thd <- R.refEmpty
  -- TODO figure out why reinit doesnt work
  conn <- R.refCreate rv (acqConn env)
  thd <- R.refCreate rv (acqThd conn dom)
  let st = St env rv dom conn thd
  -- reinitRefs st
  pure st

cleanRefs :: St -> IO ()
cleanRefs st = do
  void (R.refCleanup (stThd st))
  void (R.refCleanup (stConn st))

reinitRefs :: St -> IO ()
reinitRefs st = do
  cleanRefs st
  R.refReplace (stConn st) (acqConn (stEnv st))
  R.refReplace (stThd st) (acqThd (stConn st) (stDom st))

disposeSt :: St -> IO ()
disposeSt = R.relVarDispose . stRel

withSt :: (St -> IO a) -> IO a
withSt = bracket (initSt defaultEnv) disposeSt

advanceThread :: Ref OscConn -> Domain -> IO (Maybe ())
advanceThread conn dom = do
  now <- currentTime @PosixTime
  mr <- atomically $ do
    playing <- readTVar (domPlaying dom)
    if playing
      then fmap Just (advanceCycle dom now)
      else pure Nothing
  maybe (pure ()) (void . sendPlay conn) mr
  pure Nothing

sendPkt :: Ref OscConn -> Packet -> IO ()
sendPkt conn pkt = R.refUse conn $ \case
  Nothing -> error "Not connected"
  Just (OscConn targetAddr (Conn _ enc)) -> do
    runEncoder enc targetAddr pkt

withTimeout :: TimeDelta -> IO a -> IO (Either SomeException a)
withTimeout td act = do
  thread <- async act
  _ <- forkFinally (threadDelayDelta td) (const (cancel thread))
  waitCatch thread

recvPkt :: St -> IO (Either SomeException Packet)
recvPkt st = R.refUse (stConn st) $ \case
  Nothing -> error "Not connected"
  Just (OscConn _ (Conn dec _)) ->
    withTimeout (envOscTimeout (stEnv st)) $
      runDecoder dec >>= either throwIO pure . snd

sendHandshake :: Ref OscConn -> IO ()
sendHandshake conn = sendPkt conn O.handshakePkt

sendPlay :: Ref OscConn -> O.PlayRecord -> IO Bool
sendPlay conn pr =
  case O.playPkt pr of
    Left err -> throwIO err
    Right Nothing -> pure False
    Right (Just pkt) -> True <$ sendPkt conn pkt

testHandshake :: IO ()
testHandshake = do
  putStrLn "handshake - initializing"
  withSt $ \st -> do
    putStrLn "sending handshake"
    sendHandshake (stConn st)
    putStrLn "listening"
    resp <- recvPkt st
    putStrLn "received"
    print resp

testPlay :: IO ()
testPlay = do
  putStrLn "play - initializing"
  withSt $ \st -> do
    dawn <- currentTime
    putStrLn ("sending play @ " <> show dawn)
    let cps = 1 % 2
    _ <-
      sendPlay (stConn st) $
        O.PlayRecord dawn cps $
          B.tapeSingleton $
            B.Ev (T.Span (T.Arc 0 1) (Just (T.Arc 0 1))) $
              Map.fromList
                [ ("sound", DatumString "tabla")
                , ("orbit", DatumInt32 0)
                ]
    putStrLn "done"

testLoop :: IO ()
testLoop = do
  withSt $ \st -> do
    let tdv = domAhead (stDom st)
    _ <- R.refCreate (stRel st) $ R.acquireLoop tdv $ do
      putStrLn "hello"
      pure Nothing
    threadDelayDelta (timeDeltaFromFracSecs @Double 2)

testRecord :: IO ()
testRecord = do
  withSt $ \st -> do
    setPat st $
      pure $
        Map.fromList
          [ ("s", DatumString "tabla")
          ]
    let tdv = domAhead (stDom st)
    _ <- R.refCreate (stRel st) $ R.acquireLoop tdv $ do
      now <- currentTime
      r <- atomically (advanceCycle (stDom st) now)
      print r
      pure Nothing
    threadDelayDelta (timeDeltaFromFracSecs @Double 3)
