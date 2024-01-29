module Minipat.Dirt.Release
  ( RelVar
  , relVarInit
  , relVarDispose
  , relVarAcquire
  , withRelVar
  , acquireAsync
  , NonPosTimeDeltaErr (..)
  , acquireLoop
  )
where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM.TVar (TVar, readTVarIO)
import Control.Exception (Exception, bracket, mask, throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (InternalState, closeInternalState, createInternalState)
import Control.Monad.Trans.Resource.Internal (registerType)
import Data.Acquire.Internal (Acquire (..), Allocated (..), mkAcquire)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Nanotime (MonoTime (..), TimeDelta, awaitDelta, currentTime)

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

awaitTime :: TimeDelta -> IORef MonoTime -> IO ()
awaitTime td tv = do
  lastTime <- readIORef tv
  if lastTime == MonoTime 0
    then do
      curTime <- currentTime
      writeIORef tv curTime
    else do
      nextTime <- awaitDelta lastTime td
      writeIORef tv nextTime

acquireLoop :: TVar TimeDelta -> IO (Maybe a) -> Acquire (Async a)
acquireLoop tdv act = do
  tv <- liftIO (newIORef (MonoTime 0))
  let act' = do
        td <- readTVarIO tdv
        unless (td > 0) (throwIO (NonPosTimeDeltaErr td))
        awaitTime td tv
        act >>= maybe act' pure
  acquireAsync act'
