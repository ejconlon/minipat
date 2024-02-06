{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Core where

import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar, withMVar)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TQueue
  ( TQueue
  , flushTQueue
  , newTQueueIO
  , peekTQueue
  , readTQueue
  , tryPeekTQueue
  , writeTQueue
  )
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, readTVarIO, stateTVar, writeTVar)
import Control.Exception (Exception, SomeException, bracket, mask_, onException, throwIO)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Dahdit.Midi.Osc (Datum (..), Packet)
import Dahdit.Network (Conn (..), HostPort (..), resolveAddr, runDecoder, runEncoder, udpServerConn)
import Data.Acquire (Acquire)
import Data.Either (isRight)
import Data.Foldable (foldl', for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Sequence (Seq)
import Data.Text qualified as T
import Minipat.Dirt.Logger (LogAction, logError, logInfo, newLogger)
import Minipat.Dirt.Osc (Attrs, PlayEnv (..), PlayErr, Timed (..), convertTape, handshakePacket, playPacket)
import Minipat.Dirt.Resources (RelVar, acquireAsync, relVarAcquire, relVarDispose, relVarInit)
import Minipat.Print (prettyPrint)
import Minipat.Stream (Stream (..), streamRun)
import Minipat.Time (Arc (..), bpmToCps, cpsToBpm)
import Nanotime
  ( PosixTime (..)
  , TimeDelta
  , TimeLike (..)
  , awaitDelta
  , threadDelayDelta
  , timeDeltaFromFracSecs
  )
import Network.Socket qualified as NS

newtype NonPosTimeDeltaErr = NonPosTimeDeltaErr TimeDelta
  deriving stock (Eq, Ord, Show)

instance Exception NonPosTimeDeltaErr

awaitTime :: TimeDelta -> IORef PosixTime -> IO PosixTime
awaitTime delta timeVar = do
  lastTime <- readIORef timeVar
  nextTime <-
    if lastTime == PosixTime 0
      then currentTime
      else awaitDelta lastTime delta
  writeIORef timeVar nextTime
  pure nextTime

acquireLoop :: TVar TimeDelta -> (PosixTime -> IO ()) -> Acquire (Async ())
acquireLoop deltaVar act = do
  timeVar <- liftIO (newIORef (PosixTime 0))
  let act' = do
        delta <- readTVarIO deltaVar
        unless (delta > 0) (throwIO (NonPosTimeDeltaErr delta))
        time <- awaitTime delta timeVar
        act time
        act'
  acquireAsync act'

acquireAwait :: TVar Bool -> TQueue (Timed a) -> (Timed a -> IO ()) -> Acquire (Async ())
acquireAwait runVar queue act =
  let act' = do
        -- Peek at the first entry and await it
        time <- atomically $ do
          run <- readTVar runVar
          if run
            then fmap timedKey (peekTQueue queue)
            else retry
        now <- currentTime @PosixTime
        threadDelayDelta (diffTime time now)
        -- If it's still there (not cleared), act on it
        mtimed <- atomically $ do
          run <- readTVar runVar
          if run
            then do
              mtimed <- tryPeekTQueue queue
              case mtimed of
                Just timed
                  | timedKey timed == time ->
                      mtimed <$ readTQueue queue
                _ -> pure Nothing
            else pure Nothing
        maybe (pure ()) act mtimed
        act'
  in  acquireAsync act'

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
  , domOrbits :: !(TVar (Map Integer (Stream Attrs)))
  , domStream :: !(TVar (Stream Attrs))
  , domQueue :: !(TQueue (Timed Packet))
  -- TODO bound the queue
  }

newDomain :: IO Domain
newDomain =
  Domain
    <$> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO False
    <*> newTVarIO 0
    <*> newTVarIO mempty
    <*> newTVarIO mempty
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
  writeTVar (domOrbits dom) mempty
  writeTVar (domStream dom) mempty
  void (flushTQueue (domQueue dom))

getCps :: St -> IO Rational
getCps = readTVarIO . domCps . stDom

getAhead :: St -> IO TimeDelta
getAhead = readTVarIO . domAhead . stDom

getPlaying :: St -> IO Bool
getPlaying = readTVarIO . domPlaying . stDom

getStream :: St -> IO (Stream Attrs)
getStream = readTVarIO . domStream . stDom

getCycle :: St -> IO Integer
getCycle = readTVarIO . domCycle . stDom

getTempo :: St -> IO Rational
getTempo = fmap (cpsToBpm 4) . getCps

setTempo :: St -> Rational -> IO ()
setTempo st = setCps st . bpmToCps 4

setCps :: St -> Rational -> IO ()
setCps st cps' = atomically $ do
  let dom = stDom st
  writeTVar (domCps dom) cps'
  writeTVar (domAhead dom) (timeDeltaFromFracSecs (1 / cps'))

setPlaying :: St -> Bool -> IO ()
setPlaying st x = atomically (writeTVar (domPlaying (stDom st)) x)

setCycle :: St -> Integer -> IO ()
setCycle st x = atomically (writeTVar (domCycle (stDom st)) x)

-- TODO only set orbit if not present
updateOrbits :: St -> (Map Integer (Stream Attrs) -> Map Integer (Stream Attrs)) -> IO ()
updateOrbits st f = atomically $ do
  let dom = stDom st
  m' <- stateTVar (domOrbits dom) (\m -> let m' = f m in (m', m'))
  let z = foldl' (\x (o, y) -> x <> fmap (Map.insert "orbit" (DatumInt32 (fromIntegral o))) y) mempty (Map.toList m')
  writeTVar (domStream dom) z

setOrbit :: St -> Integer -> Stream Attrs -> IO ()
setOrbit st o s = updateOrbits st (Map.insert o s)

clearOrbit :: St -> Integer -> IO ()
clearOrbit st o = updateOrbits st (Map.delete o)

clearAllOrbits :: St -> IO ()
clearAllOrbits st = atomically (clearAllOrbitsSTM (stDom st))

hush :: St -> IO ()
hush st = atomically $ do
  let dom = stDom st
  clearAllOrbitsSTM dom
  flushQueueSTM dom

panic :: St -> IO ()
panic st = atomically $ do
  let dom = stDom st
  clearAllOrbitsSTM dom
  flushQueueSTM dom
  writeTVar (domPlaying dom) False

-- | Handshake with SuperDirt
-- On success set playing true; on error false
handshake :: St -> IO ()
handshake st = bracket acq rel (const (pure ()))
 where
  acq = do
    logInfo (stLogger st) "Handshaking ..."
    withMVar (stRes st) (flip sendPacket handshakePacket . resConn)
    recvPacket st
  rel resp = do
    let ok = isRight resp
    if ok
      then logInfo (stLogger st) "... handshake succeeded"
      else logError (stLogger st) "... handshake FAILED"
    setPlaying st ok

peek :: (Show a) => St -> Stream a -> IO ()
peek st s = do
  cyc <- fmap fromIntegral (getCycle st)
  let arc = Arc cyc (cyc + 1)
      evs = streamRun s arc
  prettyPrint arc
  for_ evs (prettyPrint . fmap show)

clearAllOrbitsSTM :: Domain -> STM ()
clearAllOrbitsSTM dom = do
  writeTVar (domOrbits dom) mempty
  writeTVar (domStream dom) mempty

flushQueueSTM :: Domain -> STM ()
flushQueueSTM dom = void (flushTQueue (domQueue dom))

genEventsSTM :: Domain -> PosixTime -> STM (PlayEnv, Either PlayErr (Seq (Timed Attrs)))
genEventsSTM dom now = do
  ahead <- readTVar (domAhead dom)
  cps <- readTVar (domCps dom)
  cyc <- readTVar (domCycle dom)
  stream <- readTVar (domStream dom)
  let tape = unStream stream (Arc (fromInteger cyc) (fromInteger cyc + 1))
      origin = addTime now ahead
      penv = PlayEnv origin cyc cps
      mpevs = convertTape penv tape
  pure (penv, mpevs)

advanceCycleSTM :: Domain -> STM ()
advanceCycleSTM dom = modifyTVar' (domCycle dom) (+ 1)

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
  { stLogger :: !LogAction
  , stEnv :: !Env
  , stDom :: !Domain
  , stRes :: !(MVar Resources)
  }

acqConn :: Env -> Acquire OscConn
acqConn (Env targetHp listenHp _ _) = do
  targetAddr <- liftIO (resolveAddr targetHp)
  conn <- udpServerConn Nothing listenHp
  pure (OscConn targetAddr conn)

acqGenTask :: LogAction -> Domain -> Acquire (Async ())
acqGenTask logger dom = acquireLoop (domAhead dom) (doGen logger dom)

acqSendTask :: OscConn -> Domain -> Acquire (Async ())
acqSendTask conn dom = acquireAwait (domPlaying dom) (domQueue dom) (doSend conn)

newSt :: LogAction -> Env -> IO St
newSt logger env = St logger env <$> initDomain env <*> newEmptyMVar

initRes :: St -> IO ()
initRes st = do
  disposeSt st
  rv <- relVarInit
  flip onException (relVarDispose rv) $ do
    conn <- relVarAcquire rv (acqConn (stEnv st))
    genTask <- relVarAcquire rv (acqGenTask (stLogger st) (stDom st))
    sendTask <- relVarAcquire rv (acqSendTask conn (stDom st))
    putMVar (stRes st) (Resources rv conn genTask sendTask)

initSt :: LogAction -> Env -> IO St
initSt logger env = newSt logger env >>= \st -> st <$ initRes st

disposeSt :: St -> IO ()
disposeSt st = mask_ (tryTakeMVar (stRes st) >>= maybe (pure ()) (relVarDispose . resRel))

withSt :: (St -> IO a) -> IO a
withSt f = do
  logger <- newLogger
  bracket (initSt logger defaultEnv) disposeSt f

doGen :: LogAction -> Domain -> PosixTime -> IO ()
doGen logger dom now = do
  mr <- atomically $ do
    playing <- readTVar (domPlaying dom)
    if playing
      then fmap Just (genEventsSTM dom now)
      else pure Nothing
  case mr of
    Nothing -> pure ()
    Just (_, mpevs) ->
      case mpevs of
        Left err -> logError logger (T.pack ("Gen failed: " ++ show err))
        Right pevs -> do
          atomically $ do
            advanceCycleSTM dom
            for_ pevs (writeTQueue (domQueue dom) . fmap playPacket)

doSend :: OscConn -> Timed Packet -> IO ()
doSend conn (Timed _ val) = do sendPacket conn val

sendPacket :: OscConn -> Packet -> IO ()
sendPacket (OscConn targetAddr (Conn _ enc)) = runEncoder enc targetAddr

withTimeout :: TimeDelta -> IO a -> IO (Either SomeException a)
withTimeout td act = do
  thread <- async act
  _ <- forkFinally (threadDelayDelta td) (const (cancel thread))
  waitCatch thread

recvPacket :: St -> IO (Either SomeException Packet)
recvPacket st = withMVar (stRes st) $ \res -> do
  let OscConn _ (Conn dec _) = resConn res
  withTimeout (envOscTimeout (stEnv st)) $
    runDecoder dec >>= either throwIO pure . snd
