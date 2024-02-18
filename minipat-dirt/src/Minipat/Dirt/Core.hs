{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Core where

import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (Async, async, cancel, poll, waitCatch)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar, withMVar)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue, newTQueueIO, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, readTVarIO, stateTVar, writeTVar)
import Control.Exception (Exception (..), SomeException, bracket, mask_, onException, throwIO)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Dahdit.Midi.Osc (Datum (..), Packet)
import Dahdit.Network (Conn (..), HostPort (..), resolveAddr, runDecoder, runEncoder, udpServerConn)
import Data.Acquire (Acquire)
import Data.Either (isRight)
import Data.Foldable (foldl', for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Minipat.Dirt.Attrs (Attrs, attrsInsert)
import Minipat.Dirt.Logger (LogAction, logDebug, logError, logInfo, logWarn, newLogger)
import Minipat.Dirt.Osc (PlayEnv (..), PlayErr, convertTape, handshakePacket, playPacket)
import Minipat.Dirt.Resources (RelVar, Timed (..), acquireAwait, acquireLoop, relVarAcquire, relVarDispose, relVarInit)
import Minipat.EStream (EStream (..))
import Minipat.Print (prettyPrint, prettyPrintAll, prettyShow, prettyShowAll)
import Minipat.Stream (Stream, streamRun, tapeToList)
import Minipat.Time (Arc (..), CycleTime (..), bpmToCps, cpsToBpm)
import Nanotime
  ( PosixTime (..)
  , TimeDelta
  , TimeLike (..)
  , threadDelayDelta
  , timeDeltaFromFracSecs
  )
import Network.Socket qualified as NS
import Prettyprinter (Pretty)

data Env = Env
  { envTargetHp :: !HostPort
  , envListenHp :: !HostPort
  , envDebug :: !Bool
  , envCps :: !Rational
  , envGpc :: !Integer
  , envOscTimeout :: !TimeDelta
  }
  deriving stock (Eq, Ord, Show)

defaultEnv :: Env
defaultEnv =
  Env
    { envTargetHp = HostPort (Just "127.0.0.1") 57120
    , envListenHp = HostPort (Just "127.0.0.1") 57129
    , envDebug = False
    , envCps = 1 % 2 -- 120 bpm, 4 bpc
    , envGpc = 8 -- Number of gens per cycle
    , envOscTimeout = timeDeltaFromFracSecs @Double 0.1
    }

data Domain = Domain
  { domDebug :: !(TVar Bool)
  , domCps :: !(TVar Rational)
  , domGpc :: !(TVar Integer)
  , domAhead :: !(TVar TimeDelta)
  , domPlaying :: !(TVar Bool)
  , domGenCycle :: !(TVar Integer)
  , domAbsGenCycle :: !(TVar Integer)
  , domOrbits :: !(TVar (Map Integer (Stream Attrs)))
  , domStream :: !(TVar (Stream Attrs))
  , domQueue :: !(TQueue (Timed Packet))
  -- TODO bound the queue
  }

newDomain :: IO Domain
newDomain =
  Domain
    <$> newTVarIO False
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO False
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO mempty
    <*> newTVarIO mempty
    <*> newTQueueIO

initDomain :: Env -> IO Domain
initDomain env = newDomain >>= \d -> d <$ reinitDomain env d

reinitDomain :: Env -> Domain -> IO ()
reinitDomain env dom = atomically $ do
  let cps = envCps env
      gpc = envGpc env
      ahead = timeDeltaFromFracSecs (1 / (cps * fromInteger gpc))
  writeTVar (domDebug dom) (envDebug env)
  writeTVar (domCps dom) cps
  writeTVar (domGpc dom) gpc
  writeTVar (domAhead dom) ahead
  writeTVar (domPlaying dom) False
  writeTVar (domGenCycle dom) 0
  writeTVar (domAbsGenCycle dom) 0
  writeTVar (domOrbits dom) mempty
  writeTVar (domStream dom) mempty
  void (flushTQueue (domQueue dom))

getDebug :: St -> IO Bool
getDebug = readTVarIO . domDebug . stDom

getCps :: St -> IO Rational
getCps = readTVarIO . domCps . stDom

getGpc :: St -> IO Integer
getGpc = readTVarIO . domGpc . stDom

getAhead :: St -> IO TimeDelta
getAhead = readTVarIO . domAhead . stDom

getPlaying :: St -> IO Bool
getPlaying = readTVarIO . domPlaying . stDom

getStream :: St -> IO (Stream Attrs)
getStream = readTVarIO . domStream . stDom

getGenCycle :: St -> IO Integer
getGenCycle = readTVarIO . domGenCycle . stDom

getAbsGenCycle :: St -> IO Integer
getAbsGenCycle = readTVarIO . domAbsGenCycle . stDom

getCycle :: St -> IO Integer
getCycle st = atomically $ do
  let dom = stDom st
  gpc <- readTVar (domGpc dom)
  gcyc <- readTVar (domGenCycle dom)
  pure (div gcyc gpc)

getTempo :: St -> IO Rational
getTempo = fmap (cpsToBpm 4) . getCps

setDebug :: St -> Bool -> IO ()
setDebug st = atomically . writeTVar (domDebug (stDom st))

setTempo :: St -> Rational -> IO ()
setTempo st = setCps st . bpmToCps 4

setCps :: St -> Rational -> IO ()
setCps st cps' = atomically $ do
  let dom = stDom st
  gpc <- readTVar (domGpc dom)
  let ahead' = timeDeltaFromFracSecs (1 / (cps' * fromInteger gpc))
  writeTVar (domCps dom) cps'
  writeTVar (domAhead dom) ahead'

setPlaying :: St -> Bool -> IO ()
setPlaying st x = atomically (writeTVar (domPlaying (stDom st)) x)

setCycle :: St -> Integer -> IO ()
setCycle st x = atomically $ do
  let dom = stDom st
  gpc <- readTVar (domGpc dom)
  gcyc <- readTVar (domGenCycle dom)
  let y = x * gpc + mod gcyc gpc
  writeTVar (domGenCycle (stDom st)) y

-- TODO only set orbit if not present
updateOrbits :: St -> (Map Integer (Stream Attrs) -> Map Integer (Stream Attrs)) -> IO ()
updateOrbits st f = atomically $ do
  let dom = stDom st
  m' <- stateTVar (domOrbits dom) (\m -> let m' = f m in (m', m'))
  let z = foldl' (\x (o, y) -> x <> fmap (attrsInsert "orbit" (DatumInt32 (fromIntegral o))) y) mempty (Map.toList m')
  writeTVar (domStream dom) z

setOrbit :: St -> Integer -> EStream Attrs -> IO ()
setOrbit st o es =
  case unEStream es of
    Left e -> putStrLn (displayException e)
    Right s -> updateOrbits st (Map.insert o s)

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
  logger = stLogger st
  acq = do
    logInfo logger "Handshaking ..."
    withMVar (stRes st) (flip sendPacket handshakePacket . resConn)
    recvPacket st
  rel resp = do
    let ok = isRight resp
    if ok
      then logInfo logger "... handshake succeeded"
      else logError logger "... handshake FAILED"
    setPlaying st ok

peek :: (Pretty a) => St -> EStream a -> IO ()
peek st es =
  case unEStream es of
    Left e -> putStrLn (displayException e)
    Right s -> do
      cyc <- fmap fromIntegral (getCycle st)
      let arc = Arc cyc (cyc + 1)
          evs = tapeToList (streamRun s arc)
      prettyPrint arc
      prettyPrintAll "\n" evs

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
  gpc <- readTVar (domGpc dom)
  gcyc <- readTVar (domGenCycle dom)
  let start = CycleTime (gcyc % gpc)
      end = CycleTime ((gcyc + 1) % gpc)
      arc = Arc start end
  stream <- readTVar (domStream dom)
  let tape = streamRun stream arc
      origin = addTime now ahead
      penv = PlayEnv origin start cps
      mpevs = convertTape penv tape
  pure (penv, mpevs)

advanceCycleSTM :: Domain -> STM ()
advanceCycleSTM dom = do
  modifyTVar' (domGenCycle dom) (+ 1)
  modifyTVar' (domAbsGenCycle dom) (+ 1)

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
acqConn (Env targetHp listenHp _ _ _ _) = do
  targetAddr <- liftIO (resolveAddr targetHp)
  conn <- udpServerConn Nothing listenHp
  pure (OscConn targetAddr conn)

acqGenTask :: LogAction -> Domain -> Acquire (Async ())
acqGenTask logger dom = acquireLoop (domAhead dom) (doGen logger dom)

acqSendTask :: OscConn -> Domain -> Acquire (Async ())
acqSendTask conn dom = acquireAwait (domPlaying dom) (domQueue dom) (doSend conn)

newSt :: LogAction -> Env -> IO St
newSt logger env = St logger env <$> initDomain env <*> newEmptyMVar

logAsyncState :: LogAction -> Text -> Async () -> IO ()
logAsyncState logger name task = do
  mea <- poll task
  case mea of
    Nothing -> logInfo logger ("Task " <> name <> " is running")
    Just ea ->
      case ea of
        Left e -> logError logger ("Task " <> name <> " failed:\n" <> T.pack (displayException e))
        Right _ -> logWarn logger ("Task " <> name <> " not running")

checkTasks :: St -> IO ()
checkTasks st = do
  let logger = stLogger st
  withMVar (stRes st) $ \res -> do
    logAsyncState logger "gen" (resGenTask res)
    logAsyncState logger "send" (resSendTask res)

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

logEvents :: LogAction -> Domain -> Seq (Timed Attrs) -> IO ()
logEvents logger dom pevs =
  unless (Seq.null pevs) $ do
    gpc <- readTVarIO (domGpc dom)
    gcyc <- readTVarIO (domGenCycle dom)
    let start = CycleTime (gcyc % gpc)
        end = CycleTime ((gcyc + 1) % gpc)
        arc = Arc start end
    logDebug logger ("Generated @ " <> prettyShow arc <> "\n" <> prettyShowAll "\n" pevs)

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
          debug <- readTVarIO (domDebug dom)
          when debug (logEvents logger dom pevs)
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
