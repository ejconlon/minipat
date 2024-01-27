module Minipat.Dirt.Ref
  ( RelVar
  , relVarInit
  , relVarDispose
  , withRelVar
  , Ref
  , refPure
  , refEmpty
  , refCreate
  , refCreate'
  , refCleanup
  , refReplace
  , refUse
  , acquireAsync
  , NonPosTimeDeltaErr (..)
  , acquireLoop
  , RefM
  , refRead
  , refMayRead
  , refRun
  )
where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO, stateTVar, writeTVar)
import Control.Exception (Exception, bracket, bracket_, mask, mask_, throwIO)
import Control.Monad (ap, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (InternalState, ReleaseKey, closeInternalState, createInternalState)
import Control.Monad.Trans.Resource.Internal (registerType)
import Data.Acquire.Internal (Acquire (..), Allocated (..), ReleaseType (..), mkAcquire)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Nanotime (MonoTime (..), TimeDelta, awaitDelta, currentTime)

type RelVar = InternalState

relVarInit :: IO RelVar
relVarInit = createInternalState

relVarDispose :: RelVar -> IO ()
relVarDispose = closeInternalState

withRelVar :: (RelVar -> IO a) -> IO a
withRelVar = bracket relVarInit relVarDispose

-- private
data X a
  = XOpen !(Allocated a)
  | XLocked
  | XClosing
  | XClosed
  | XDisposed

data Ref a = Ref
  { refKey :: !ReleaseKey
  , refVar :: !(TVar (X a))
  }

instance Eq (Ref a) where
  Ref _ v1 == Ref _ v2 = v1 == v2

-- private
mkRef :: RelVar -> TVar (X a) -> IO (Ref a)
mkRef rv var = do
  key <- registerType rv (\rt -> void (refCleanupWith rt XDisposed var))
  pure (Ref key var)

refPure :: RelVar -> a -> IO (Ref a)
refPure rv a = newTVarIO (XOpen (Allocated a (const (pure ())))) >>= mkRef rv

refEmpty :: RelVar -> IO (Ref a)
refEmpty rv = newTVarIO XClosed >>= mkRef rv

refCreate :: RelVar -> Acquire a -> IO (Ref a)
refCreate rv (Acquire f) = mask $ \restore -> do
  allo <- f restore
  var <- newTVarIO (XOpen allo)
  mkRef rv var

refCreate' :: RelVar -> IO a -> (a -> IO ()) -> IO (Ref a)
refCreate' rv acq rel = refCreate rv (mkAcquire acq rel)

-- | Release the ref, returning True if this was the releaser.
-- False if early return due to other thread releasing.
refCleanup :: Ref a -> IO Bool
refCleanup = refCleanupWith ReleaseEarly XClosed . refVar

-- private
refCleanupWith :: ReleaseType -> X a -> TVar (X a) -> IO Bool
refCleanupWith rt doneVal var = do
  xvar <- newTVarIO Nothing
  let bacq = atomically $ do
        x <- readTVar var
        mx <- case x of
          XOpen _ -> Just x <$ writeTVar var XClosing
          XLocked -> retry
          _ -> pure Nothing
        writeTVar xvar mx
      brel = atomically (writeTVar var doneVal)
      use = do
        mx <- readTVarIO xvar
        case mx of
          Just (XOpen (Allocated _ rel)) -> True <$ rel rt
          _ -> pure False
  bracket_ bacq brel use

refReplace :: Ref a -> Acquire a -> IO ()
refReplace (Ref _ var) (Acquire f) = do
  xvar <- newTVarIO Nothing
  let bacq = do
        -- First lock var
        atomically $ do
          x <- readTVar var
          mx <- case x of
            XOpen _ -> Just x <$ writeTVar var XLocked
            XLocked -> retry
            XClosing -> retry
            XClosed -> Nothing <$ writeTVar var XLocked
            XDisposed -> error "Replacing a disposed ref"
          writeTVar xvar mx
        -- Then cleanup existing ref
        mask_ $ do
          mx <- atomically (stateTVar xvar (,Just XClosed))
          case mx of
            Just (XOpen (Allocated _ rel)) -> do
              rel ReleaseEarly
            _ -> pure ()
      brel = atomically $ do
        mx <- readTVar xvar
        maybe (pure ()) (writeTVar var) mx
      use = mask $ \restore -> do
        allo <- f restore
        atomically (writeTVar xvar (Just (XOpen allo)))
  bracket_ bacq brel use

refUse :: Ref a -> (Maybe a -> IO b) -> IO b
refUse (Ref _ var) f = bracket bacq brel use
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

acquireAsync :: IO a -> Acquire (Async a)
acquireAsync act = mkAcquire (async act) cancel

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

acquireLoop :: TVar TimeDelta -> IO (Maybe a) -> Acquire (Async a)
acquireLoop tdv act = do
  tv <- liftIO (newIORef (MonoTime 0))
  let act' = do
        td <- readTVarIO tdv
        unless (td > 0) (throwIO NonPosTimeDeltaErr)
        awaitTime td tv
        act >>= maybe act' pure
  acquireAsync act'

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
refReadWith f (Ref _ var) = RefM $ \k -> do
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
