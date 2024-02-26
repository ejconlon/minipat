module Minipat.Live.Resources
  ( RelVar
  , relVarInit
  , relVarDispose
  , relVarUse
  , relVarAcquire
  , withRelVar
  , acquireAsync
  , acquireLoop
  , QueueHead
  , qhTQueue
  , qhHeap
  , acquireAwait
  , threadDelayUntil
  , withTimeout
  )
where

import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TQueue (TQueue, peekTQueue, readTQueue, tryPeekTQueue)
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Control.Exception (SomeException, bracket, mask, onException)
import Control.Monad.Trans.Resource (InternalState, closeInternalState, createInternalState)
import Control.Monad.Trans.Resource.Internal (registerType)
import Data.Acquire.Internal (Acquire (..), Allocated (..), mkAcquire)
import Data.Heap (Heap)
import Data.Heap qualified as H
import Nanotime (PosixTime (..), TimeDelta, currentTime, diffTime, threadDelayDelta)

type RelVar = InternalState

relVarInit :: IO RelVar
relVarInit = createInternalState

relVarDispose :: RelVar -> IO ()
relVarDispose = closeInternalState

relVarUse :: (RelVar -> IO a) -> IO a
relVarUse f = do
  rv <- relVarInit
  onException (f rv) (relVarDispose rv)

relVarAcquire :: RelVar -> Acquire a -> IO a
relVarAcquire rv (Acquire f) = mask $ \restore -> do
  Allocated a free <- f restore
  _ <- registerType rv free
  return a

withRelVar :: (RelVar -> IO a) -> IO a
withRelVar = bracket relVarInit relVarDispose

acquireAsync :: IO a -> Acquire (Async a)
acquireAsync act = mkAcquire (async act) cancel

acquireLoop :: (PosixTime -> IO PosixTime) -> PosixTime -> Acquire (Async ())
acquireLoop act now0 =
  let go now = do
        next <- act now
        threadDelayUntil next
        go next
  in  acquireAsync (go now0)

data QueueHead v = QueueHead
  { qhPeek :: !(STM v)
  , qhTryPeek :: !(STM (Maybe v))
  , qhRead :: !(STM v)
  }

qhTQueue :: TQueue v -> QueueHead v
qhTQueue q = QueueHead (peekTQueue q) (tryPeekTQueue q) (readTQueue q)

qhHeap :: TVar (Heap v) -> QueueHead v
qhHeap hv = QueueHead peekH tryPeekH readH
 where
  peekH = do
    h <- readTVar hv
    case H.uncons h of
      Just (v, _) -> pure v
      Nothing -> retry
  tryPeekH = do
    h <- readTVar hv
    pure (fmap fst (H.uncons h))
  readH = do
    h <- readTVar hv
    case H.uncons h of
      Just (v, h') -> v <$ writeTVar hv h'
      Nothing -> retry

acquireAwait :: (v -> PosixTime) -> STM Bool -> QueueHead v -> (v -> IO ()) -> Acquire (Async ())
acquireAwait getTime getRunning qh act =
  let go = do
        -- Peek at the first entry and await it
        target <- atomically $ do
          run <- getRunning
          if run
            then fmap getTime (qhPeek qh)
            else retry
        threadDelayUntil target
        -- If something actionable is still there, act on it
        mv <- atomically $ do
          run <- getRunning
          if run
            then do
              mv <- qhTryPeek qh
              case mv of
                Just v
                  | getTime v <= target ->
                      mv <$ qhRead qh
                _ -> pure Nothing
            else pure Nothing
        maybe (pure ()) act mv
        go
  in  acquireAsync go

threadDelayUntil :: PosixTime -> IO ()
threadDelayUntil target =
  currentTime >>= threadDelayDelta . diffTime target

withTimeout :: TimeDelta -> IO a -> IO (Either SomeException a)
withTimeout delta act = do
  thread <- async act
  _ <- forkFinally (threadDelayDelta delta) (const (cancel thread))
  waitCatch thread
