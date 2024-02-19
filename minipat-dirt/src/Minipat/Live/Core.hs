{-# LANGUAGE OverloadedStrings #-}

module Minipat.Live.Core where

import Control.Concurrent.Async (Async, poll)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar, withMVar)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue, newTQueueIO)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, readTVarIO, stateTVar, writeTVar)
import Control.Exception (Exception (..), mask_)
import Control.Monad (unless, void)
import Dahdit.Midi.Osc (Datum (..))
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
import Minipat.Live.Resources (RelVar, Timed (..), relVarDispose, relVarUse)
import Minipat.Print (prettyPrint, prettyPrintAll, prettyShow, prettyShowAll)
import Minipat.Stream (Stream, streamRun, tapeToList)
import Minipat.Time (Arc (..), CycleTime (..), bpmToCps, cpsToBpm)
import Nanotime
  ( TimeDelta
  , timeDeltaFromFracSecs
  )
import Prettyprinter (Pretty)

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

data Domain y = Domain
  { domDebug :: !(TVar Bool)
  , domCps :: !(TVar Rational)
  , domGpc :: !(TVar Integer)
  , domAhead :: !(TVar TimeDelta)
  , domPlaying :: !(TVar Bool)
  , domGenCycle :: !(TVar Integer)
  , domAbsGenCycle :: !(TVar Integer)
  , domOrbits :: !(TVar (Map Integer (Stream Attrs)))
  , domStream :: !(TVar (Stream Attrs))
  , domQueue :: !(TQueue (Timed y))
  -- TODO bound the queue
  }

newDomain :: IO (Domain y)
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

initDomain :: CommonEnv -> IO (Domain y)
initDomain ce = newDomain >>= \d -> d <$ reinitDomain ce d

reinitDomain :: CommonEnv -> Domain y -> IO ()
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
  void (flushTQueue (domQueue dom))

type Spawner i d y = LogAction -> Domain y -> RelVar -> i -> IO (Map Text (Async ()), d)

newtype Impl i d y = Impl
  { implSpawn :: Spawner i d y
  }

data Resources d = Resources
  { resRel :: !RelVar
  , resTasks :: !(Map Text (Async ()))
  , resData :: !d
  }

data St i d y = St
  { stLogger :: !LogAction
  , stImpl :: !(Impl i d y)
  , stEnv :: !(Env i)
  , stDom :: !(Domain y)
  , stRes :: !(MVar (Resources d))
  }

newSt :: LogAction -> Impl i d y -> Env i -> IO (St i d y)
newSt logger impl env = St logger impl env <$> initDomain (envCommon env) <*> newEmptyMVar

initRes :: St i d y -> IO ()
initRes st = do
  disposeSt st
  relVarUse $ \rv -> do
    (tasks, dat) <- implSpawn (stImpl st) (stLogger st) (stDom st) rv (envImpl (stEnv st))
    putMVar (stRes st) (Resources rv tasks dat)

initSt :: LogAction -> Impl i d y -> Env i -> IO (St i d y)
initSt logger impl env = newSt logger impl env >>= \st -> st <$ initRes st

disposeSt :: St i d y -> IO ()
disposeSt st = mask_ (tryTakeMVar (stRes st) >>= maybe (pure ()) (relVarDispose . resRel))

getDebug :: St i d y -> IO Bool
getDebug = readTVarIO . domDebug . stDom

getCps :: St i d y -> IO Rational
getCps = readTVarIO . domCps . stDom

getGpc :: St i d y -> IO Integer
getGpc = readTVarIO . domGpc . stDom

getAhead :: St i d y -> IO TimeDelta
getAhead = readTVarIO . domAhead . stDom

getPlaying :: St i d y -> IO Bool
getPlaying = readTVarIO . domPlaying . stDom

getStream :: St i d y -> IO (Stream Attrs)
getStream = readTVarIO . domStream . stDom

getGenCycle :: St i d y -> IO Integer
getGenCycle = readTVarIO . domGenCycle . stDom

getAbsGenCycle :: St i d y -> IO Integer
getAbsGenCycle = readTVarIO . domAbsGenCycle . stDom

getCycle :: St i d y -> IO Integer
getCycle st = atomically $ do
  let dom = stDom st
  gpc <- readTVar (domGpc dom)
  gcyc <- readTVar (domGenCycle dom)
  pure (div gcyc gpc)

getTempo :: St i d y -> IO Rational
getTempo = fmap (cpsToBpm 4) . getCps

setDebug :: St i d y -> Bool -> IO ()
setDebug st = atomically . writeTVar (domDebug (stDom st))

setTempo :: St i d y -> Rational -> IO ()
setTempo st = setCps st . bpmToCps 4

setCps :: St i d y -> Rational -> IO ()
setCps st cps' = atomically $ do
  let dom = stDom st
  gpc <- readTVar (domGpc dom)
  let ahead' = timeDeltaFromFracSecs (1 / (cps' * fromInteger gpc))
  writeTVar (domCps dom) cps'
  writeTVar (domAhead dom) ahead'

setPlaying :: St i d y -> Bool -> IO ()
setPlaying st x = atomically (writeTVar (domPlaying (stDom st)) x)

setCycle :: St i d y -> Integer -> IO ()
setCycle st x = atomically $ do
  let dom = stDom st
  gpc <- readTVar (domGpc dom)
  gcyc <- readTVar (domGenCycle dom)
  let y = x * gpc + mod gcyc gpc
  writeTVar (domGenCycle (stDom st)) y

updateOrbits :: St i d y -> (Map Integer (Stream Attrs) -> Map Integer (Stream Attrs)) -> IO ()
updateOrbits st f = atomically $ do
  let dom = stDom st
      addOrbit = attrsDefault "orbit" . DatumInt32 . fromInteger
  m' <- stateTVar (domOrbits dom) (\m -> let m' = f m in (m', m'))
  let z = foldl' (\x (o, y) -> x <> fmap (addOrbit o) y) mempty (Map.toList m')
  writeTVar (domStream dom) z

setOrbit :: St i d y -> Integer -> EStream Attrs -> IO ()
setOrbit st o es =
  case unEStream es of
    Left e -> putStrLn (displayException e)
    Right s -> updateOrbits st (Map.insert o s)

clearOrbit :: St i d y -> Integer -> IO ()
clearOrbit st o = updateOrbits st (Map.delete o)

clearAllOrbits :: St i d y -> IO ()
clearAllOrbits st = atomically (clearAllOrbitsSTM (stDom st))

hush :: St i d y -> IO ()
hush st = atomically $ do
  let dom = stDom st
  clearAllOrbitsSTM dom
  flushQueueSTM dom

panic :: St i d y -> IO ()
panic st = atomically $ do
  let dom = stDom st
  clearAllOrbitsSTM dom
  flushQueueSTM dom
  writeTVar (domPlaying dom) False

peek :: (Pretty a) => St i d y -> EStream a -> IO ()
peek st es =
  case unEStream es of
    Left e -> putStrLn (displayException e)
    Right s -> do
      cyc <- fmap fromIntegral (getCycle st)
      let arc = Arc cyc (cyc + 1)
          evs = tapeToList (streamRun s arc)
      prettyPrint arc
      prettyPrintAll "\n" evs

clearAllOrbitsSTM :: Domain y -> STM ()
clearAllOrbitsSTM dom = do
  writeTVar (domOrbits dom) mempty
  writeTVar (domStream dom) mempty

flushQueueSTM :: Domain y -> STM ()
flushQueueSTM dom = void (flushTQueue (domQueue dom))

advanceCycleSTM :: Domain y -> STM ()
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

checkTasks :: St i d y -> IO ()
checkTasks st =
  withMVar (stRes st) $ \res ->
    for_ (Map.toList (resTasks res)) (uncurry (logAsyncState (stLogger st)))

logEvents :: (Pretty a) => LogAction -> Domain y -> Seq (Timed a) -> IO ()
logEvents logger dom pevs =
  unless (Seq.null pevs) $ do
    gpc <- readTVarIO (domGpc dom)
    gcyc <- readTVarIO (domGenCycle dom)
    let start = CycleTime (gcyc % gpc)
        end = CycleTime ((gcyc + 1) % gpc)
        arc = Arc start end
    logDebug logger ("Generated @ " <> prettyShow arc <> "\n" <> prettyShowAll "\n" pevs)

withData :: St i d y -> (d -> IO a) -> IO a
withData st f = withMVar (stRes st) (f . resData)
