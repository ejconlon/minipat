{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Prelude where

import Control.Applicative (empty)
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (async, cancel, waitCatch)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (SomeException, bracket, throwIO)
import Control.Monad.IO.Class (liftIO)
import Dahdit.Midi.Osc (Datum (..), Packet)
import Dahdit.Network (Conn (..), HostPort (..), resolveAddr, runDecoder, runEncoder, udpServerConn)
import Data.Acquire (Acquire)
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Minipat.Base qualified as B
import Minipat.Dirt.Osc qualified as O
import Minipat.Dirt.Ref (Ref, ReleaseVar)
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
  { domDawn :: !(TVar PosixTime)
  , domCps :: !(TVar Rational)
  , domAhead :: !(TVar TimeDelta)
  , domPlaying :: !(TVar Bool)
  , domCycle :: !(TVar Integer)
  , domPat :: !(TVar (B.Pat O.OscMap))
  }

newDomain :: IO Domain
newDomain =
  Domain
    <$> newTVarIO (PosixTime 0)
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO False
    <*> newTVarIO 0
    <*> newTVarIO empty

initDomain :: Env -> IO Domain
initDomain env = newDomain >>= \d -> d <$ reinitDomain env d

reinitDomain :: Env -> Domain -> IO ()
reinitDomain env dom = do
  now <- currentTime
  let cps = envCps env
      td = timeDeltaFromFracSecs (1 / cps)
  atomically $ do
    writeTVar (domDawn dom) now
    writeTVar (domCps dom) cps
    writeTVar (domAhead dom) td
    writeTVar (domPlaying dom) False
    writeTVar (domCycle dom) 0
    writeTVar (domPat dom) empty

getDawn :: St -> IO PosixTime
getDawn = readTVarIO . domDawn . stDom

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
  dawn <- readTVar (domDawn dom)
  cps <- readTVar (domCps dom)
  cyc <- readTVar (domCycle dom)
  let dawn' = addTime dawn (timeDeltaFromFracSecs (fromInteger cyc * (cps' - cps)))
  writeTVar (domDawn dom) dawn'
  writeTVar (domCps dom) cps'
  writeTVar (domAhead dom) (timeDeltaFromFracSecs (1 / cps'))

setPlaying :: St -> Bool -> IO ()
setPlaying st x = atomically (writeTVar (domPlaying (stDom st)) x)

setPat :: St -> B.Pat O.OscMap -> IO ()
setPat st x = atomically (writeTVar (domPat (stDom st)) x)

setCycle :: St -> Integer -> IO ()
setCycle st cyc' = atomically $ do
  let dom = stDom st
  dawn <- readTVar (domDawn dom)
  cps <- readTVar (domCps dom)
  cyc <- readTVar (domCycle dom)
  let dawn' = addTime dawn (timeDeltaFromFracSecs (cps * fromInteger (cyc' - cyc)))
  writeTVar (domDawn dom) dawn'
  writeTVar (domCycle dom) cyc'

data Record = Record
  { recDawn :: !PosixTime
  , recCps :: !Rational
  , recTape :: !(B.Tape O.OscMap)
  }
  deriving stock (Eq, Ord, Show)

advanceCycle :: St -> IO Record
advanceCycle st = atomically $ do
  let dom = stDom st
  dawn <- readTVar (domDawn dom)
  cps <- readTVar (domCps dom)
  cyc <- fmap fromInteger (readTVar (domCycle dom))
  pat <- readTVar (domPat dom)
  let tape = B.unPat pat (T.Arc cyc (cyc + 1))
  modifyTVar' (domCycle dom) (+ 1)
  pure (Record dawn cps tape)

data OscConn = OscConn
  { ocTargetAddr :: !NS.SockAddr
  , ocListenConn :: !(Conn NS.SockAddr)
  }

data St = St
  { stEnv :: !Env
  , stRel :: !ReleaseVar
  , stDom :: !Domain
  , stConn :: !(Ref OscConn)
  }

acqConn :: Env -> Acquire OscConn
acqConn (Env targetHp listenHp _ _) = do
  targetAddr <- liftIO (resolveAddr targetHp)
  conn <- udpServerConn Nothing listenHp
  pure (OscConn targetAddr conn)

initSt :: Env -> IO St
initSt env = do
  rv <- R.releaseVarCreate
  ref <- R.refCreate rv (acqConn env)
  dom <- initDomain env
  pure (St env rv dom ref)

reinitSt :: St -> IO ()
reinitSt st = R.refReplace (stConn st) (acqConn (stEnv st))

cleanupSt :: St -> IO ()
cleanupSt = R.releaseVarCleanup . stRel

withSt :: (St -> IO a) -> IO a
withSt = bracket (initSt defaultEnv) cleanupSt

sendPkt :: St -> Packet -> IO ()
sendPkt (St _ _ _ ref) pkt = R.refUse ref $ \case
  Nothing -> error "Not connected"
  Just (OscConn targetAddr (Conn _ enc)) -> do
    runEncoder enc targetAddr pkt

withTimeout :: TimeDelta -> IO a -> IO (Either SomeException a)
withTimeout td act = do
  thread <- async act
  _ <- forkFinally (threadDelayDelta td) (const (cancel thread))
  waitCatch thread

recvPkt :: St -> IO (Either SomeException Packet)
recvPkt (St (Env _ _ _ timeout) _ _ ref) = R.refUse ref $ \case
  Nothing -> error "Not connected"
  Just (OscConn _ (Conn dec _)) ->
    withTimeout timeout $
      runDecoder dec >>= either throwIO pure . snd

sendHandshake :: St -> IO ()
sendHandshake st = sendPkt st O.handshakePkt

sendPlay :: St -> PosixTime -> Rational -> B.Tape O.OscMap -> IO Bool
sendPlay st dawn cps tape =
  case O.playPkt dawn cps tape of
    Left err -> throwIO err
    Right Nothing -> pure False
    Right (Just pkt) -> True <$ sendPkt st pkt

testHandshake :: IO ()
testHandshake = do
  putStrLn "handshake - initializing"
  withSt $ \st -> do
    putStrLn "sending handshake"
    sendHandshake st
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
      sendPlay st dawn cps $
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
    _ <- R.refLoop (stRel st) tdv $ do
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
    _ <- R.refLoop (stRel st) tdv $ do
      r <- advanceCycle st
      print r
      pure Nothing
    threadDelayDelta (timeDeltaFromFracSecs @Double 3)
