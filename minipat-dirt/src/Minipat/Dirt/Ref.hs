module Minipat.Dirt.Ref
  ( ReleaseVar
  , releaseVarCreate
  , releaseVarCleanup
  , Ref
  , refPure
  , refEmpty
  , refCreate
  , refCreate'
  , refReplace
  , refUse
  , refAsync
  , NonPosTimeDeltaErr (..)
  , refLoop
  , RefM
  , refRead
  , refMayRead
  , refRun
  )
where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (Exception, bracket, bracket_, mask, throwIO)
import Control.Monad (ap, unless, void)
import Control.Monad.Trans.Resource (createInternalState)
import Control.Monad.Trans.Resource.Internal (ReleaseMap, registerType, stateCleanup)
import Data.Acquire.Internal (Acquire (..), Allocated (..), ReleaseType (..), mkAcquire)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Nanotime (MonoTime (..), TimeDelta (..), awaitDelta, currentTime)

type ReleaseVar = IORef ReleaseMap

releaseVarCreate :: IO ReleaseVar
releaseVarCreate = createInternalState

releaseVarCleanup :: ReleaseVar -> IO ()
releaseVarCleanup = stateCleanup ReleaseNormal

data X a
  = XOpen !(Allocated a)
  | XLocked
  | XClosing
  | XClosed

newtype Ref a = Ref {unRef :: TVar (X a)}
  deriving stock (Eq)

refPure :: a -> IO (Ref a)
refPure = fmap Ref . newTVarIO . XOpen . (`Allocated` const (pure ()))

refEmpty :: IO (Ref a)
refEmpty = fmap Ref (newTVarIO XClosed)

refCreate :: ReleaseVar -> Acquire a -> IO (Ref a)
refCreate rv (Acquire f) = mask $ \restore -> do
  allo <- f restore
  ref <- fmap Ref (newTVarIO (XOpen allo))
  _ <- registerType rv (\rt -> void (refCleanupWith rt ref))
  pure ref

refCreate' :: ReleaseVar -> IO a -> (a -> IO ()) -> IO (Ref a)
refCreate' rv acq rel = refCreate rv (mkAcquire acq rel)

-- | Release the ref, returning True if this was the releaser.
-- False if early return due to other thread releasing.
refCleanup :: Ref a -> IO Bool
refCleanup = refCleanupWith ReleaseEarly

refCleanupWith :: ReleaseType -> Ref a -> IO Bool
refCleanupWith rt (Ref var) = do
  xvar <- newIORef Nothing
  let bacq = do
        mx <- atomically $ do
          x <- readTVar var
          case x of
            XOpen _ -> Just x <$ writeTVar var XClosing
            XLocked -> retry
            _ -> pure Nothing
        writeIORef xvar mx
      brel = do
        mx <- readIORef xvar
        case mx of
          Just (XOpen _) -> atomically (writeTVar var XClosed)
          _ -> pure ()
      use = do
        mx <- readIORef xvar
        case mx of
          Just (XOpen (Allocated _ rel)) -> True <$ rel rt
          _ -> pure False
  bracket_ bacq brel use

refReplace :: Ref a -> Acquire a -> IO ()
refReplace (Ref var) (Acquire f) = do
  xvar <- newIORef Nothing
  let bacq = do
        mx <- atomically $ do
          x <- readTVar var
          case x of
            XOpen _ -> Just x <$ writeTVar var XLocked
            XLocked -> retry
            XClosing -> retry
            XClosed -> Nothing <$ writeTVar var XLocked
        writeIORef xvar mx
      brel = do
        mx <- readIORef xvar
        case mx of
          Nothing -> pure ()
          Just x -> atomically (writeTVar var x)
      use = do
        mx <- atomicModifyIORef' xvar (,Just XClosed)
        case mx of
          Just (XOpen (Allocated _ rel)) -> rel ReleaseEarly
          _ -> pure ()
        mask $ \restore -> do
          allo <- f restore
          writeIORef xvar (Just (XOpen allo))
  bracket_ bacq brel use

refUse :: Ref a -> (Maybe a -> IO b) -> IO b
refUse (Ref var) f = bracket bacq brel use
 where
  bacq = atomically $ do
    x <- readTVar var
    case x of
      XOpen _ -> Just x <$ writeTVar var XLocked
      XLocked -> retry
      _ -> pure Nothing
  brel = maybe (pure ()) (atomically . writeTVar var)
  use =
    f . \case
      Just (XOpen (Allocated a _)) -> Just a
      _ -> Nothing

refAsync :: ReleaseVar -> IO a -> IO (Ref (Async a))
refAsync rv act = refCreate' rv (async act) cancel

data NonPosTimeDeltaErr = NonPosTimeDeltaErr
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

refLoop :: ReleaseVar -> TVar TimeDelta -> IO (Maybe a) -> IO (Ref (Async a))
refLoop rv tdv act = do
  tv <- newIORef (MonoTime 0)
  let act' = do
        td@(TimeDelta x) <- readTVarIO tdv
        unless (x > 0) (throwIO NonPosTimeDeltaErr)
        awaitTime td tv
        act >>= maybe act' pure
  refAsync rv act'

newtype RefM a = RefM {unRefM :: forall b. (Maybe a -> STM () -> STM (b, STM ())) -> STM (b, STM ())}
  deriving stock (Functor)

instance Applicative RefM where
  pure a = RefM (\k -> k (Just a) (pure ()))
  (<*>) = ap

instance Monad RefM where
  return = pure
  RefM y >>= f = RefM $ \k -> y $ \ma r ->
    case ma of
      Nothing -> k Nothing r
      Just a -> let (RefM z) = f a in z (\mb q -> k mb (q >> r))

refReadWith :: (Maybe a -> Maybe b) -> Ref a -> RefM b
refReadWith f (Ref var) = RefM $ \k -> do
  x <- readTVar var
  case x of
    XOpen (Allocated a _) -> writeTVar var XLocked *> k (f (Just a)) (writeTVar var x)
    XLocked -> retry
    _ -> k (f Nothing) (pure ())

refRead :: Ref a -> RefM a
refRead = refReadWith id

refMayRead :: Ref a -> RefM (Maybe a)
refMayRead = refReadWith Just

refRun :: RefM a -> (Maybe a -> IO b) -> IO b
refRun (RefM y) f = bracket bacq rel use
 where
  bacq = atomically (y (curry pure))
  rel (_, r) = atomically r
  use (ma, _) = f ma
