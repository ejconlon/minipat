module Minipat.Dirt.Resources
  ( RelVar
  , relVarInit
  , relVarDispose
  , relVarAcquire
  , withRelVar
  , acquireAsync
  )
where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Exception (bracket, mask)
import Control.Monad.Trans.Resource (InternalState, closeInternalState, createInternalState)
import Control.Monad.Trans.Resource.Internal (registerType)
import Data.Acquire.Internal (Acquire (..), Allocated (..), mkAcquire)

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
