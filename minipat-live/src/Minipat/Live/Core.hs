{-# LANGUAGE OverloadedStrings #-}

module Minipat.Live.Core
  ( Env (..)
  , St
  , newSt
  , initRes
  , stLogger
  , stEnv
  , stBackend
  , initAsyncSt
  , disposeSt
  , useCallback
  , getCps
  , getAhead
  , getPlaying
  , getStream
  , getCycle
  , getCycleArc
  , getTempo
  , setCps
  , setPlaying
  , setCycle
  , setTempo
  , setOrbit
  , setOrbit'
  , clearOrbit
  , clearAllOrbits
  , hush
  , panic
  , checkTasks
  , logAsyncState
  , peek
  , stepRecord
  , mergeRecord
  , simpleRecord
  )
where

import Control.Concurrent.Async (Async, async, poll)
import Control.Concurrent.MVar (MVar, modifyMVarMasked_, modifyMVar_, newMVar, withMVar)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, readTVarIO, stateTVar, writeTVar)
import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Acquire (Acquire)
import Data.Default (Default (..))
import Data.Foldable (foldl')
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Minipat.EStream (EStream (..))
import Minipat.Live.Attrs (Attrs, IsAttrs (..))
import Minipat.Live.Backend (Backend (..), Callback (..), UninitErr (..), WithPlayMeta)
import Minipat.Live.Exception (catchUserErr)
import Minipat.Live.Logger (LogAction, logDebug, logException, logInfo, logWarn, nullLogger)
import Minipat.Live.Play (PlayEnv (..), PlayErr, WithOrbit (..), playTape)
import Minipat.Live.Resources (RelVar, acquireLoop, relVarAcquire, relVarDispose, relVarUse)
import Minipat.Print (prettyShow, prettyShowAll)
import Minipat.Stream (Stream, streamRun, tapeToList)
import Minipat.Time (Arc (..), CycleArc, CycleTime (..), bpmToCps, cpsToBpm)
import Nanotime
  ( PosixTime
  , TimeDelta
  , addTime
  , currentTime
  , timeDeltaFromFracSecs
  )
import Prettyprinter (Pretty)

-- * Environment

data Env = Env
  { envDebug :: !Bool
  , envCps :: !Rational
  , envGpc :: !Integer
  }
  deriving stock (Eq, Ord, Show)

instance Default Env where
  def =
    Env
      { envDebug = False
      , envCps = 1 % 2 -- 120 bpm, 4 bpc
      , envGpc = 16 -- Number of gens per cycle
      }

-- * State

data Domain = Domain
  { domDebug :: !(TVar Bool)
  , domCps :: !(TVar Rational)
  , domGpc :: !(TVar Integer)
  , domPlaying :: !(TVar Bool)
  , domGenCycle :: !(TVar Integer)
  , domAbsGenCycle :: !(TVar Integer)
  , domOrbits :: !(TVar (Map Integer (Stream Attrs)))
  , domStream :: !(TVar (Stream (WithOrbit Attrs)))
  }

newDomain :: IO Domain
newDomain =
  Domain
    <$> newTVarIO False
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO False
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO mempty
    <*> newTVarIO mempty

initDomain :: Env -> IO Domain
initDomain env = newDomain >>= \d -> d <$ reinitDomain env d

reinitDomain :: Env -> Domain -> IO ()
reinitDomain (Env debug cps gpc) dom = atomically $ do
  writeTVar (domDebug dom) debug
  writeTVar (domCps dom) cps
  writeTVar (domGpc dom) gpc
  writeTVar (domPlaying dom) False
  writeTVar (domGenCycle dom) 0
  writeTVar (domAbsGenCycle dom) 0
  writeTVar (domOrbits dom) mempty
  writeTVar (domStream dom) mempty

data Resources d = Resources
  { resRel :: !RelVar
  , resGenTask :: !(Async ())
  , resData :: !d
  }

data St i = St
  { stLogger :: !LogAction
  , stBackend :: !i
  , stEnv :: !Env
  , stDom :: !Domain
  , stRes :: !(MVar (Maybe (Resources (BackendData i))))
  }

newSt :: LogAction -> i -> Env -> IO (St i)
newSt logger be env = St logger be env <$> initDomain env <*> newMVar Nothing

initRes :: (Backend i) => Bool -> St i -> IO ()
initRes isSync st = do
  disposeSt st
  relVarUse $ \rv -> do
    let getPlayingSTM = readTVar (domPlaying (stDom st))
    dat <- relVarAcquire rv (backendInit (stBackend st) (stLogger st) getPlayingSTM)
    genTask <-
      if isSync
        then liftIO (async (pure ()))
        else do
          now <- currentTime
          relVarAcquire rv (acqGenTask st now)
    modifyMVar_ (stRes st) (const (pure (Just (Resources rv genTask dat))))
  setPlaying st True

initSt :: (Backend i) => Bool -> LogAction -> i -> Env -> IO (St i)
initSt isSync logger be env = newSt logger be env >>= \st -> st <$ initRes isSync st

initAsyncSt :: (Backend i) => LogAction -> i -> Env -> IO (St i)
initAsyncSt = initSt False

initSyncSt :: (Backend i) => LogAction -> i -> Env -> IO (St i)
initSyncSt = initSt True

stepGenSt
  :: St i
  -> PosixTime
  -> IO (CycleTime, PosixTime, Seq (WithPlayMeta Attrs))
stepGenSt st now = do
  (penv, mresult) <- atomically (genAndAdvanceSTM (stDom st) now)
  let cycleBounds = peCycleBounds penv
  events <- case mresult of
    Nothing -> pure Empty
    Just eresult -> case eresult of
      Left err -> Empty <$ logException (stLogger st) ("Error @ " <> prettyShow cycleBounds) err
      Right events -> pure events
  let nextCycTime = arcEnd cycleBounds
      nextRealTime = peRealOrigin penv
  pure (nextCycTime, nextRealTime, events)

stepSendSt :: (Backend i) => St i -> Seq (WithPlayMeta Attrs) -> IO ()
stepSendSt st evs = catchUserErr act report
 where
  act = backendSend (stBackend st) (stLogger st) (mkCallback st) evs
  report err = do
    logException (stLogger st) "Error on send" err
    debug <- readTVarIO (domDebug (stDom st))
    when debug (setPlaying st False)

disposeSt :: St i -> IO ()
disposeSt st = do
  setPlaying st False
  modifyMVarMasked_ (stRes st) $ \case
    Nothing -> pure Nothing
    Just res -> Nothing <$ relVarDispose (resRel res)

mkCallback :: St i -> Callback (BackendData i)
mkCallback st = Callback (useCallback st)

useCallback :: St i -> (BackendData i -> IO r) -> IO r
useCallback st f = withMVar (stRes st) (maybe (throwIO UninitErr) (f . resData))

-- * Getters

getCps :: St i -> IO Rational
getCps = readTVarIO . domCps . stDom

getGpc :: St i -> IO Integer
getGpc = readTVarIO . domGpc . stDom

getAhead :: St i -> IO TimeDelta
getAhead = atomically . getAheadSTM . stDom

getPlaying :: St i -> IO Bool
getPlaying = readTVarIO . domPlaying . stDom

getStream :: St i -> IO (Stream (WithOrbit Attrs))
getStream = readTVarIO . domStream . stDom

getGenCycle :: St i -> IO Integer
getGenCycle = readTVarIO . domGenCycle . stDom

getAbsGenCycle :: St i -> IO Integer
getAbsGenCycle = readTVarIO . domAbsGenCycle . stDom

getCycle :: St i -> IO Integer
getCycle st = atomically $ do
  let dom = stDom st
  gpc <- readTVar (domGpc dom)
  gcyc <- readTVar (domGenCycle dom)
  pure (div gcyc gpc)

getCycleArc :: St i -> IO CycleArc
getCycleArc = atomically . getCycleArcSTM . stDom

getTempo :: St i -> IO Rational
getTempo = fmap (cpsToBpm 4) . getCps

-- * Setters

setTempo :: St i -> Rational -> IO ()
setTempo st = setCps st . bpmToCps 4

setCps :: St i -> Rational -> IO ()
setCps st = atomically . writeTVar (domCps (stDom st))

setPlaying :: St i -> Bool -> IO ()
setPlaying st x = atomically (writeTVar (domPlaying (stDom st)) x)

setCycle :: St i -> Integer -> IO ()
setCycle st x = atomically $ do
  let dom = stDom st
  gpc <- readTVar (domGpc dom)
  gcyc <- readTVar (domGenCycle dom)
  let y = x * gpc + mod gcyc gpc
  writeTVar (domGenCycle (stDom st)) y

updateOrbits :: St i -> (Map Integer (Stream Attrs) -> Map Integer (Stream Attrs)) -> IO ()
updateOrbits st f = atomically $ do
  let dom = stDom st
  m' <- stateTVar (domOrbits dom) (\m -> let m' = f m in (m', m'))
  let z = foldl' (\x (o, y) -> x <> fmap (WithOrbit o) y) mempty (Map.toList m')
  writeTVar (domStream dom) z

setOrbit :: (IsAttrs a) => St i -> Integer -> EStream a -> IO ()
setOrbit st o es =
  case unEStream es of
    Left e -> logException (stLogger st) ("Error setting orbit " <> T.pack (show o)) e
    Right s -> setOrbit' st o s

setOrbit' :: (IsAttrs a) => St i -> Integer -> Stream a -> IO ()
setOrbit' st o s = updateOrbits st (Map.insert o (fmap toAttrs s))

clearOrbit :: St i -> Integer -> IO ()
clearOrbit st o = updateOrbits st (Map.delete o)

clearAllOrbits :: St i -> IO ()
clearAllOrbits st = atomically (clearAllOrbitsSTM (stDom st))

clearEvents :: (Backend i) => St i -> IO ()
clearEvents st = backendClear (stBackend st) (stLogger st) (mkCallback st)

-- * Other actions

hush :: (Backend i) => St i -> IO ()
hush st = do
  atomically (clearAllOrbitsSTM (stDom st))
  clearEvents st

panic :: (Backend i) => St i -> IO ()
panic st = do
  atomically $ do
    let dom = stDom st
    clearAllOrbitsSTM dom
    writeTVar (domPlaying dom) False
  clearEvents st

peek :: (Pretty a) => St i -> EStream a -> IO ()
peek st es =
  let logger = stLogger st
  in  case unEStream es of
        Left e -> logException logger "Error peeking" e
        Right s -> do
          cyc <- fmap fromIntegral (getCycle st)
          let arc = Arc cyc (cyc + 1)
              evs = tapeToList (streamRun s arc)
          logInfo logger (prettyShow arc <> "\n" <> prettyShowAll "\n" evs)

-- * Recording

data RecordBackend = RecordBackend
  deriving stock (Eq, Ord, Show)

instance Default RecordBackend where
  def = RecordBackend

newtype RecordData = RecordData {unRecordData :: IORef (Seq (WithPlayMeta Attrs))}

newRecordData :: IO RecordData
newRecordData = fmap RecordData (newIORef Empty)

flushRecordData :: RecordData -> IO (Seq (WithPlayMeta Attrs))
flushRecordData (RecordData r) = atomicModifyIORef' r (Empty,)

instance Backend RecordBackend where
  type BackendData RecordBackend = RecordData

  backendInit _ _ _ = liftIO newRecordData
  backendSend _ _ cb vs = runCallback cb (\(RecordData r) -> modifyIORef' r (<> vs))
  backendClear _ _ cb = runCallback cb (\(RecordData r) -> modifyIORef' r (const Empty))
  backendCheck _ _ _ = pure True

stepRecord
  :: Env
  -> Integer
  -> Integer
  -> PosixTime
  -> (CycleTime -> PosixTime -> St RecordBackend -> IO ())
  -> (CycleTime -> PosixTime -> Seq (WithPlayMeta Attrs) -> IO ())
  -> IO PosixTime
stepRecord env start end now0 onEnter onLeave = go
 where
  go = do
    st <- initSyncSt nullLogger RecordBackend env
    setCycle st start
    setPlaying st True
    loop (fromInteger end) st now0
  loop cycEnd st !realNow = do
    Arc cycNow _ <- getCycleArc st
    onEnter cycNow realNow st
    (cycNext, realNext, events) <- stepGenSt st realNow
    onLeave cycNow realNow events
    if cycNext >= cycEnd
      then pure realNext
      else loop cycEnd st realNext

mergeRecord
  :: Env
  -> Integer
  -> Integer
  -> PosixTime
  -> (St RecordBackend -> IO ())
  -> IO (Seq (WithPlayMeta Attrs), PosixTime)
mergeRecord ce start end now0 onInit = do
  let cycStart = fromInteger start
  r <- newIORef Empty
  realEnd <-
    stepRecord
      ce
      start
      end
      now0
      (\cyc _ -> when (cyc == cycStart) . onInit)
      (\_ _ tas -> modifyIORef' r (<> tas))
  tas <- readIORef r
  pure (tas, realEnd)

simpleRecord
  :: (St RecordBackend -> IO ())
  -> IO (Seq (WithPlayMeta Attrs))
simpleRecord = fmap fst . mergeRecord def 0 1 0

-- Helpers

getCycleArcSTM :: Domain -> STM CycleArc
getCycleArcSTM dom = do
  gpc <- readTVar (domGpc dom)
  gcyc <- readTVar (domGenCycle dom)
  let start = CycleTime (gcyc % gpc)
      end = CycleTime ((gcyc + 1) % gpc)
  pure (Arc start end)

getRealOriginSTM :: Domain -> PosixTime -> STM PosixTime
getRealOriginSTM dom now = do
  ahead <- getAheadSTM dom
  pure (addTime now ahead)

clearAllOrbitsSTM :: Domain -> STM ()
clearAllOrbitsSTM dom = do
  writeTVar (domOrbits dom) mempty
  writeTVar (domStream dom) mempty

advanceCycleSTM :: Domain -> STM ()
advanceCycleSTM dom = do
  modifyTVar' (domGenCycle dom) (+ 1)
  modifyTVar' (domAbsGenCycle dom) (+ 1)

logAsyncState :: LogAction -> Text -> Async () -> IO Bool
logAsyncState logger name task = do
  mea <- poll task
  case mea of
    Nothing -> True <$ logInfo logger ("Task " <> name <> " is running")
    Just ea ->
      case ea of
        Left e -> False <$ logException logger ("Task " <> name <> " failed") e
        Right _ -> False <$ logWarn logger ("Task " <> name <> " not running")

checkTasks :: (Backend i) => St i -> IO Bool
checkTasks st =
  let logger = stLogger st
  in  withMVar (stRes st) $ \case
        Nothing -> pure False
        Just res -> do
          genOk <- logAsyncState logger "gen" (resGenTask res)
          backOk <- backendCheck (stBackend st) logger (mkCallback st)
          pure (genOk && backOk)

logEvents :: (Pretty q) => LogAction -> Domain -> Seq (WithPlayMeta q) -> IO ()
logEvents logger dom pevs =
  unless (Seq.null pevs) $ do
    arc <- atomically (getCycleArcSTM dom)
    logDebug logger ("Generated @ " <> prettyShow arc <> "\n" <> prettyShowAll "\n" pevs)

getPlayEnvSTM :: Domain -> PosixTime -> STM PlayEnv
getPlayEnvSTM dom now = do
  realOrigin <- getRealOriginSTM dom now
  cycleBounds <- getCycleArcSTM dom
  cps <- readTVar (domCps dom)
  pure (PlayEnv realOrigin cycleBounds cps)

genAndAdvanceSTM :: Domain -> PosixTime -> STM (PlayEnv, Maybe (Either PlayErr (Seq (WithPlayMeta Attrs))))
genAndAdvanceSTM dom now = do
  penv <- getPlayEnvSTM dom now
  playing <- readTVar (domPlaying dom)
  mresult <-
    if playing
      then do
        stream <- readTVar (domStream dom)
        let arc = peCycleBounds penv
            tape = streamRun stream arc
            result = playTape penv tape
        advanceCycleSTM dom
        pure (Just result)
      else pure Nothing
  pure (penv, mresult)

getAheadSTM :: Domain -> STM TimeDelta
getAheadSTM dom = do
  cps <- readTVar (domCps dom)
  gpc <- readTVar (domGpc dom)
  pure (timeDeltaFromFracSecs (1 / (cps * fromInteger gpc)))

runGenTask :: (Backend i) => St i -> PosixTime -> IO PosixTime
runGenTask st now = do
  (_, next, events) <- stepGenSt st now
  stepSendSt st events
  pure next

acqGenTask :: (Backend i) => St i -> PosixTime -> Acquire (Async ())
acqGenTask = acquireLoop . runGenTask
