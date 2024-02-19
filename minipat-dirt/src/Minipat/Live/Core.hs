{-# LANGUAGE OverloadedStrings #-}

module Minipat.Live.Core
  ( Env (..)
  , defaultEnv
  , ImplInit
  , ImplSend
  , Impl (..)
  , St
  , stLogger
  , stEnv
  , initAsyncSt
  , initSyncSt
  , disposeSt
  , stepSt
  , multiStepSt
  , withData
  , getDebug
  , getCps
  , getAhead
  , getPlaying
  , getStream
  , getCycle
  , getTempo
  , setDebug
  , setCps
  , setPlaying
  , setCycle
  , setTempo
  , setOrbit
  , clearOrbit
  , clearAllOrbits
  , hush
  , panic
  , checkTasks
  , peek
  )
where

import Control.Concurrent.Async (Async, poll)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar, withMVar)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue, newTQueueIO, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, readTVarIO, stateTVar, writeTVar)
import Control.Exception (Exception (..), mask_)
import Control.Monad (unless, void, when)
import Dahdit.Midi.Osc (Datum (..))
import Data.Acquire (Acquire)
import Data.Foldable (foldl', for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Minipat.EStream (EStream (..))
import Minipat.Live.Attrs (Attrs, attrsDefault)
import Minipat.Live.Logger (LogAction, logDebug, logError, logInfo, logWarn)
import Minipat.Live.Osc (PlayEnv (..), PlayErr, convertTape)
import Minipat.Live.Resources (RelVar, Timed (..), acquireAwait, acquireLoop, relVarAcquire, relVarDispose, relVarUse)
import Minipat.Print (prettyPrint, prettyPrintAll, prettyShow, prettyShowAll)
import Minipat.Stream (Stream, streamRun, tapeToList)
import Minipat.Time (Arc (..), CycleTime (..), bpmToCps, cpsToBpm)
import Nanotime
  ( PosixTime
  , TimeDelta
  , addTime
  , timeDeltaFromFracSecs
  )
import Prettyprinter (Pretty)

-- * Environment

data CommonEnv = CommonEnv
  { ceDebug :: !Bool
  , ceCps :: !Rational
  , ceGpc :: !Integer
  }
  deriving stock (Eq, Ord, Show)

defaultCommonEnv :: CommonEnv
defaultCommonEnv =
  CommonEnv
    { ceDebug = False
    , ceCps = 1 % 2 -- 120 bpm, 4 bpc
    , ceGpc = 8 -- Number of gens per cycle
    }

data Env i = Env
  { envCommon :: !CommonEnv
  , envImpl :: !i
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

defaultEnv :: i -> Env i
defaultEnv = Env defaultCommonEnv

-- * Impl

type ImplInit i d = LogAction -> RelVar -> i -> IO d

type ImplSend d = LogAction -> ((d -> IO ()) -> IO ()) -> Timed Attrs -> IO ()

data Impl i d = Impl
  { implInit :: !(ImplInit i d)
  , implSend :: !(ImplSend d)
  }

-- * State

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
  , domEvents :: !(TQueue (Timed Attrs))
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

initDomain :: CommonEnv -> IO Domain
initDomain ce = newDomain >>= \d -> d <$ reinitDomain ce d

reinitDomain :: CommonEnv -> Domain -> IO ()
reinitDomain ce dom = atomically $ do
  let cps = ceCps ce
      gpc = ceGpc ce
      ahead = timeDeltaFromFracSecs (1 / (cps * fromInteger gpc))
  writeTVar (domDebug dom) (ceDebug ce)
  writeTVar (domCps dom) cps
  writeTVar (domGpc dom) gpc
  writeTVar (domAhead dom) ahead
  writeTVar (domPlaying dom) False
  writeTVar (domGenCycle dom) 0
  writeTVar (domAbsGenCycle dom) 0
  writeTVar (domOrbits dom) mempty
  writeTVar (domStream dom) mempty
  clearEventsSTM dom

data Tasks = Tasks
  { taskGen :: !(Async ())
  , taskSend :: !(Async ())
  }

data Resources d = Resources
  { resRel :: !RelVar
  , resTasks :: !(Maybe Tasks)
  , resData :: !d
  }

data St i d = St
  { stLogger :: !LogAction
  , stImpl :: !(Impl i d)
  , stEnv :: !(Env i)
  , stDom :: !Domain
  , stRes :: !(MVar (Resources d))
  }

newSt :: LogAction -> Impl i d -> Env i -> IO (St i d)
newSt logger impl env = St logger impl env <$> initDomain (envCommon env) <*> newEmptyMVar

initRes :: Bool -> St i d -> IO ()
initRes isSync st = do
  disposeSt st
  relVarUse $ \rv -> do
    dat <- implInit (stImpl st) (stLogger st) rv (envImpl (stEnv st))
    tasks <-
      if isSync
        then pure Nothing
        else do
          genTask <- relVarAcquire rv (acqGenTask st)
          sendTask <- relVarAcquire rv (acqSendTask st)
          pure (Just (Tasks genTask sendTask))
    putMVar (stRes st) (Resources rv tasks dat)

initSt :: Bool -> LogAction -> Impl i d -> Env i -> IO (St i d)
initSt isSync logger impl env = newSt logger impl env >>= \st -> st <$ initRes isSync st

initAsyncSt :: LogAction -> Impl i d -> Env i -> IO (St i d)
initAsyncSt = initSt False

initSyncSt :: LogAction -> Impl i d -> Env i -> IO (St i d)
initSyncSt = initSt True

stepSt :: St i d -> PosixTime -> IO (CycleTime, PosixTime)
stepSt st now = do
  let dom = stDom st
      wd = withData st
      send = implSend (stImpl st) (stLogger st) wd
  doGen (stLogger st) dom now
  (events, nextCycTime, ahead) <- atomically $ do
    events <- flushTQueue (domEvents dom)
    nextCycTime <- getCycleTimeSTM dom
    ahead <- readTVar (domAhead dom)
    pure (events, nextCycTime, ahead)
  for_ events send
  let nextNow = addTime now ahead
  pure (nextCycTime, nextNow)

multiStepSt :: St i d -> Integer -> PosixTime -> IO (CycleTime, PosixTime)
multiStepSt st numCycs now0 = go
 where
  go = do
    cyc0 <- getCycleTime st
    if numCycs <= 0
      then pure (cyc0, now0)
      else do
        let end = cyc0 + fromInteger numCycs
        loop end now0
  loop end now = do
    p@(cyc, next) <- stepSt st now
    if cyc >= end
      then pure p
      else loop end next

disposeSt :: St i d -> IO ()
disposeSt st = mask_ (tryTakeMVar (stRes st) >>= maybe (pure ()) (relVarDispose . resRel))

withData :: St i d -> (d -> IO a) -> IO a
withData st f = withMVar (stRes st) (f . resData)

-- * Getters

getDebug :: St i d -> IO Bool
getDebug = readTVarIO . domDebug . stDom

getCps :: St i d -> IO Rational
getCps = readTVarIO . domCps . stDom

getGpc :: St i d -> IO Integer
getGpc = readTVarIO . domGpc . stDom

getAhead :: St i d -> IO TimeDelta
getAhead = readTVarIO . domAhead . stDom

getPlaying :: St i d -> IO Bool
getPlaying = readTVarIO . domPlaying . stDom

getStream :: St i d -> IO (Stream Attrs)
getStream = readTVarIO . domStream . stDom

getGenCycle :: St i d -> IO Integer
getGenCycle = readTVarIO . domGenCycle . stDom

getAbsGenCycle :: St i d -> IO Integer
getAbsGenCycle = readTVarIO . domAbsGenCycle . stDom

getCycle :: St i d -> IO Integer
getCycle st = atomically $ do
  let dom = stDom st
  gpc <- readTVar (domGpc dom)
  gcyc <- readTVar (domGenCycle dom)
  pure (div gcyc gpc)

getCycleTime :: St i d -> IO CycleTime
getCycleTime = atomically . getCycleTimeSTM . stDom

getTempo :: St i d -> IO Rational
getTempo = fmap (cpsToBpm 4) . getCps

-- * Setters

setDebug :: St i d -> Bool -> IO ()
setDebug st = atomically . writeTVar (domDebug (stDom st))

setTempo :: St i d -> Rational -> IO ()
setTempo st = setCps st . bpmToCps 4

setCps :: St i d -> Rational -> IO ()
setCps st cps' = atomically $ do
  let dom = stDom st
  gpc <- readTVar (domGpc dom)
  let ahead' = timeDeltaFromFracSecs (1 / (cps' * fromInteger gpc))
  writeTVar (domCps dom) cps'
  writeTVar (domAhead dom) ahead'

setPlaying :: St i d -> Bool -> IO ()
setPlaying st x = atomically (writeTVar (domPlaying (stDom st)) x)

setCycle :: St i d -> Integer -> IO ()
setCycle st x = atomically $ do
  let dom = stDom st
  gpc <- readTVar (domGpc dom)
  gcyc <- readTVar (domGenCycle dom)
  let y = x * gpc + mod gcyc gpc
  writeTVar (domGenCycle (stDom st)) y

updateOrbits :: St i d -> (Map Integer (Stream Attrs) -> Map Integer (Stream Attrs)) -> IO ()
updateOrbits st f = atomically $ do
  let dom = stDom st
      addOrbit = attrsDefault "orbit" . DatumInt32 . fromInteger
  m' <- stateTVar (domOrbits dom) (\m -> let m' = f m in (m', m'))
  let z = foldl' (\x (o, y) -> x <> fmap (addOrbit o) y) mempty (Map.toList m')
  writeTVar (domStream dom) z

setOrbit :: St i d -> Integer -> EStream Attrs -> IO ()
setOrbit st o es =
  case unEStream es of
    Left e -> putStrLn (displayException e)
    Right s -> updateOrbits st (Map.insert o s)

clearOrbit :: St i d -> Integer -> IO ()
clearOrbit st o = updateOrbits st (Map.delete o)

clearAllOrbits :: St i d -> IO ()
clearAllOrbits st = atomically (clearAllOrbitsSTM (stDom st))

-- * Other actions

hush :: St i d -> IO ()
hush st = atomically $ do
  let dom = stDom st
  clearAllOrbitsSTM dom
  clearEventsSTM dom

panic :: St i d -> IO ()
panic st = atomically $ do
  let dom = stDom st
  clearAllOrbitsSTM dom
  clearEventsSTM dom
  writeTVar (domPlaying dom) False

peek :: (Pretty a) => St i d -> EStream a -> IO ()
peek st es =
  case unEStream es of
    Left e -> putStrLn (displayException e)
    Right s -> do
      cyc <- fmap fromIntegral (getCycle st)
      let arc = Arc cyc (cyc + 1)
          evs = tapeToList (streamRun s arc)
      prettyPrint arc
      prettyPrintAll "\n" evs

-- Helpers

getCycleTimeSTM :: Domain -> STM CycleTime
getCycleTimeSTM dom = do
  gpc <- readTVar (domGpc dom)
  gcyc <- readTVar (domGenCycle dom)
  pure (CycleTime (gcyc % gpc))

clearAllOrbitsSTM :: Domain -> STM ()
clearAllOrbitsSTM dom = do
  writeTVar (domOrbits dom) mempty
  writeTVar (domStream dom) mempty

clearEventsSTM :: Domain -> STM ()
clearEventsSTM dom = void (flushTQueue (domEvents dom))

advanceCycleSTM :: Domain -> STM ()
advanceCycleSTM dom = do
  modifyTVar' (domGenCycle dom) (+ 1)
  modifyTVar' (domAbsGenCycle dom) (+ 1)

logAsyncState :: LogAction -> Text -> Async () -> IO ()
logAsyncState logger name task = do
  mea <- poll task
  case mea of
    Nothing -> logInfo logger ("Task " <> name <> " is running")
    Just ea ->
      case ea of
        Left e -> logError logger ("Task " <> name <> " failed:\n" <> T.pack (displayException e))
        Right _ -> logWarn logger ("Task " <> name <> " not running")

checkTasks :: St i d -> IO ()
checkTasks st =
  withMVar (stRes st) $ \res ->
    let logger = stLogger st
    in  case resTasks res of
          Nothing -> logInfo logger "No tasks running"
          Just tasks -> do
            logAsyncState logger "gen" (taskGen tasks)
            logAsyncState logger "send" (taskSend tasks)

logEvents :: (Pretty a) => LogAction -> Domain -> Seq (Timed a) -> IO ()
logEvents logger dom pevs =
  unless (Seq.null pevs) $ do
    gpc <- readTVarIO (domGpc dom)
    gcyc <- readTVarIO (domGenCycle dom)
    let start = CycleTime (gcyc % gpc)
        end = CycleTime ((gcyc + 1) % gpc)
        arc = Arc start end
    logDebug logger ("Generated @ " <> prettyShow arc <> "\n" <> prettyShowAll "\n" pevs)

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
            for_ pevs (writeTQueue (domEvents dom))

acqGenTask :: St i d -> Acquire (Async ())
acqGenTask st =
  let logger = stLogger st
      dom = stDom st
  in  acquireLoop (domAhead dom) (doGen logger dom)

acqSendTask :: St i d -> Acquire (Async ())
acqSendTask st =
  let logger = stLogger st
      dom = stDom st
      impl = stImpl st
      wd = withData st
      doSend = implSend impl logger wd
  in  acquireAwait (domPlaying dom) (domEvents dom) doSend
