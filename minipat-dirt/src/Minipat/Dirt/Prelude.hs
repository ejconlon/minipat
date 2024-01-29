{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Prelude where

import Control.Applicative (empty)
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar, withMVar)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue, newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (SomeException, bracket, mask_, onException, throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Dahdit.Midi.Osc (Datum (..), Packet)
import Dahdit.Network (Conn (..), HostPort (..), resolveAddr, runDecoder, runEncoder, udpServerConn)
import Data.Acquire (Acquire)
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Sequence (Seq)
import Minipat.Base qualified as B
import Minipat.Dirt.Osc qualified as O
import Minipat.Dirt.Release (RelVar)
import Minipat.Dirt.Release qualified as R
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
  , domQueue :: !(TQueue O.TimedPacket)
  -- TODO bound the queue
  }

newDomain :: IO Domain
newDomain =
  Domain
    <$> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO False
    <*> newTVarIO 0
    <*> newTVarIO empty
    <*> newTQueueIO

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
  void (flushTQueue (domQueue dom))

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

getTempo :: St -> IO Rational
getTempo = fmap (T.cpsToBpm 4) . getCps

setTempo :: St -> Rational -> IO ()
setTempo st = setCps st . T.bpmToCps 4

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

readEvents :: Domain -> PosixTime -> STM (O.PlayEnv, Either O.OscErr (Seq O.PlayEvent))
readEvents dom now = do
  ahead <- readTVar (domAhead dom)
  cps <- readTVar (domCps dom)
  cyc <- readTVar (domCycle dom)
  pat <- readTVar (domPat dom)
  let tape = B.unPat pat (T.Arc (fromInteger cyc) (fromInteger cyc + 1))
      origin = addTime now ahead
      penv = O.PlayEnv origin cyc cps
      mpevs = O.convertTape penv tape
  pure (penv, mpevs)

advanceCycle :: Domain -> STM ()
advanceCycle dom = modifyTVar' (domCycle dom) (+ 1)

data OscConn = OscConn
  { ocTargetAddr :: !NS.SockAddr
  , ocListenConn :: !(Conn NS.SockAddr)
  }

data Resources = Resources
  { resRel :: !RelVar
  , resConn :: !OscConn
  , resGenTask :: !(Async ())
  , resSendTask :: !(Async ())
  }

data St = St
  { stEnv :: !Env
  , stDom :: !Domain
  , stRes :: !(MVar Resources)
  }

acqConn :: Env -> Acquire OscConn
acqConn (Env targetHp listenHp _ _) = do
  targetAddr <- liftIO (resolveAddr targetHp)
  conn <- udpServerConn Nothing listenHp
  pure (OscConn targetAddr conn)

acqGenTask :: Domain -> Acquire (Async ())
acqGenTask dom = R.acquireLoop (domAhead dom) (runGenTask dom)

acqSendTask :: OscConn -> Domain -> Acquire (Async ())
acqSendTask conn dom = R.acquireAsync (runSendTask conn dom)

initSt :: Env -> IO St
initSt env = St env <$> initDomain env <*> newEmptyMVar

initRes :: St -> IO ()
initRes st = do
  disposeSt st
  rv <- R.relVarInit
  flip onException (R.relVarDispose rv) $ do
    conn <- R.relVarAcquire rv (acqConn (stEnv st))
    genTask <- R.relVarAcquire rv (acqGenTask (stDom st))
    sendTask <- R.relVarAcquire rv (acqSendTask conn (stDom st))
    putMVar (stRes st) (Resources rv conn genTask sendTask)

disposeSt :: St -> IO ()
disposeSt st = mask_ (tryTakeMVar (stRes st) >>= maybe (pure ()) (R.relVarDispose . resRel))

withSt :: (St -> IO a) -> IO a
withSt = bracket (initSt defaultEnv) disposeSt

runGenTask :: Domain -> IO (Maybe ())
runGenTask dom = do
  now <- currentTime @PosixTime
  mr <- atomically $ do
    playing <- readTVar (domPlaying dom)
    if playing
      then fmap Just (readEvents dom now)
      else pure Nothing
  case mr of
    Nothing -> pure ()
    Just (_, mpevs) ->
      case mpevs of
        Left err -> throwIO err
        Right pevs -> atomically $ do
          advanceCycle dom
          for_ pevs (writeTQueue (domQueue dom) . O.playPacket)
  pure Nothing

runSendTask :: OscConn -> Domain -> IO ()
runSendTask conn dom = go
 where
  go = do
    tp@(O.TimedPacket tm pkt) <- atomically (readTQueue (domQueue dom))
    now <- currentTime
    print now
    print tp
    threadDelayDelta (diffTime tm now)
    sendPacket conn pkt
    go

sendPacket :: OscConn -> Packet -> IO ()
sendPacket (OscConn targetAddr (Conn _ enc)) = runEncoder enc targetAddr

withTimeout :: TimeDelta -> IO a -> IO (Either SomeException a)
withTimeout td act = do
  thread <- async act
  _ <- forkFinally (threadDelayDelta td) (const (cancel thread))
  waitCatch thread

recvPkt :: St -> IO (Either SomeException Packet)
recvPkt st = withMVar (stRes st) $ \res -> do
  let OscConn _ (Conn dec _) = resConn res
  withTimeout (envOscTimeout (stEnv st)) $
    runDecoder dec >>= either throwIO pure . snd

sendHandshake :: OscConn -> IO ()
sendHandshake conn = sendPacket conn O.handshakePacket

sendPlay :: OscConn -> Either O.OscErr (Seq O.PlayEvent) -> IO ()
sendPlay conn mpevs =
  case mpevs of
    Left err -> throwIO err
    Right pevs ->
      for_ pevs $ \pev -> do
        let tp@(O.TimedPacket tm pkt) = O.playPacket pev
        print tp
        now <- currentTime
        threadDelayDelta (diffTime now tm)
        sendPacket conn pkt

testHandshake :: IO ()
testHandshake = do
  putStrLn "handshake - initializing"
  withSt $ \st -> do
    putStrLn "sending handshake"
    withMVar (stRes st) (sendHandshake . resConn)
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
        penv = O.PlayEnv dawn 0 cps
        arg =
          O.convertTape penv $
            B.tapeSingleton $
              B.Ev (T.Span (T.Arc 0 1) (Just (T.Arc 0 1))) $
                Map.fromList
                  [ ("sound", DatumString "tabla")
                  , ("orbit", DatumInt32 0)
                  ]
    withMVar (stRes st) (\res -> sendPlay (resConn res) arg)
    putStrLn "done"

testReal :: IO ()
testReal = do
  putStrLn "real - initializing"
  withSt $ \st -> do
    let m =
          Map.fromList
            [ ("sound", DatumString "cpu")
            , ("orbit", DatumInt32 0)
            ]
    setPat st (B.patFastBy 4 (pure m))
    setPlaying st True
    threadDelayDelta (timeDeltaFromFracSecs @Double 2)
    setTempo st 180
    threadDelayDelta (timeDeltaFromFracSecs @Double 3)
    setPlaying st False
