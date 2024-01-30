module Minipat.Dirt.Resources
  ( RelVar
  , relVarInit
  , relVarDispose
  , relVarAcquire
  , withRelVar
  , acquireAsync
  , NonPosTimeDeltaErr (..)
  , acquireLoop
  , Timed (..)
  , acquireAwait
  )
where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TQueue (TQueue, peekTQueue, tryReadTQueue)
import Control.Concurrent.STM.TVar (TVar, readTVar, readTVarIO)
import Control.Exception (Exception, bracket, mask, throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (InternalState, closeInternalState, createInternalState)
import Control.Monad.Trans.Resource.Internal (registerType)
import Data.Acquire.Internal (Acquire (..), Allocated (..), mkAcquire)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Nanotime (PosixTime (..), TimeDelta, TimeLike (..), awaitDelta, currentTime, threadDelayDelta)

type RelVar = InternalState

relVarInit :: IO RelVar
relVarInit = createInternalState

relVarDispose :: RelVar -> IO ()
relVarDispose = closeInternalState

relVarAcquire :: RelVar -> Acquire a -> IO a
relVarAcquire rv (Acquire f) = mask $ \restore -> do
  Allocated a free <- f restore
  _ <- registerType rv free
  return a

withRelVar :: (RelVar -> IO a) -> IO a
withRelVar = bracket relVarInit relVarDispose

acquireAsync :: IO a -> Acquire (Async a)
acquireAsync act = mkAcquire (async act) cancel

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

data Timed a = Timed
  { timedKey :: !PosixTime
  , timedVal :: !a
  }
  deriving stock (Eq, Ord, Show)

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
        mtimed <- atomically (tryReadTQueue queue)
        maybe (pure ()) act mtimed
        act'
  in  acquireAsync act'
