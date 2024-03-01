{-# LANGUAGE OverloadedStrings #-}

-- | Much of what you need to implement a backend. If you do anything fancy,
-- you may need to import 'Minipat.Live.Core' and mess with state directly.
module Minipat.Live.Backend
  ( Callback (..)
  , Backend (..)
  , UninitErr (..)
  , PlayMeta (..)
  , pmCycleLength
  , pmRealLength
  , WithPlayMeta (..)
  )
where

import Control.Concurrent.STM (STM)
import Control.Exception (Exception)
import Data.Acquire (Acquire)
import Data.Default (Default)
import Data.Kind (Type)
import Data.Sequence (Seq)
import Minipat.Live.Attrs (Attrs)
import Minipat.Live.Logger (LogAction)
import Minipat.Time (CycleArc, CycleDelta, PosixArc, arcLength)
import Nanotime (TimeDelta)
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as P

-- * Callback

-- | Allows a backend to access its data behind a mutex
newtype Callback d = Callback {runCallback :: forall r. (d -> IO r) -> IO r}
  deriving stock (Functor)

-- * Backend

-- | 'Minipat.Live.Core' manages the system state, but uses these methods to
-- manipulate the backend-specific state. Note that values of the implementing
-- type `i` should be _immutable_ - all state should be in `BackendData i`.
class (Default i) => Backend i where
  -- | Whatever data is useful to a particular backend, e.g. async task handles,
  -- queues, network connections.
  type BackendData i :: Type

  -- | Initialize a new instance of the backend. This may be called multiple times
  -- a session, but it is guaranteed that old data will have been disposed by the
  -- time this is invoked.
  backendInit
    :: i
    -> LogAction
    -> STM Bool
    -- ^ Gets the current play state (useful for pausing event sends)
    -> Acquire (BackendData i)

  -- | Enqueue the given events for sending async. Typically a sender thread and queue
  -- will be allocated in 'backendInit' and this will use the callback to write to
  -- the queue. Any exceptions thrown (e.g. in processing nonsensical attributes)
  -- will be logged.
  backendSend
    :: i
    -> LogAction
    -> Callback (BackendData i)
    -> Seq (WithPlayMeta Attrs)
    -> IO ()

  -- | Invoked on hush - clear the sending queue (if relevant).
  backendClear
    :: i
    -> LogAction
    -> Callback (BackendData i)
    -> IO ()

  -- | Check the health of the backend and log any useful information,
  -- returning True if healthy.
  backendCheck
    :: i
    -> LogAction
    -> Callback (BackendData i)
    -> IO Bool

-- * UninitErr

-- | Thrown when using a callback associated with an uninitialized backend
data UninitErr = UninitErr
  deriving stock (Eq, Ord, Show)

instance Exception UninitErr

-- * PlayMeta

-- | Metadata around play events
data PlayMeta = PlayMeta
  { pmOrbit :: !Integer
  , pmRealArc :: !PosixArc
  , pmCycleArc :: !CycleArc
  , pmCps :: !Rational
  }
  deriving stock (Eq, Ord, Show)

instance Pretty PlayMeta where
  pretty pm = P.hcat [pretty (pmCycleArc pm), " d", pretty (pmOrbit pm)]

pmCycleLength :: PlayMeta -> CycleDelta
pmCycleLength = arcLength . pmCycleArc

pmRealLength :: PlayMeta -> TimeDelta
pmRealLength = arcLength . pmRealArc

data WithPlayMeta a = WithPlayMeta !PlayMeta !a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty a) => Pretty (WithPlayMeta a) where
  pretty (WithPlayMeta pm a) = P.hsep [pretty pm, pretty a]
