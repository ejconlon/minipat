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
import Data.Kind (Type)
import Data.Sequence (Seq)
import Minipat.Live.Logger (LogAction)
import Minipat.Time (CycleArc, CycleDelta, PosixArc, arcLength)
import Nanotime (TimeDelta)
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as P

-- * Callback

newtype Callback d = Callback {runCallback :: forall r. (d -> IO r) -> IO r}
  deriving stock (Functor)

-- * Backend

class Backend i where
  type BackendData i :: Type
  type BackendAttrs i :: Type

  backendInit
    :: i
    -> LogAction
    -> STM Bool
    -> Acquire (BackendData i)

  backendSend
    :: i
    -> LogAction
    -> Callback (BackendData i)
    -> Seq (WithPlayMeta (BackendAttrs i))
    -> IO ()

  backendClear
    :: i
    -> LogAction
    -> Callback (BackendData i)
    -> IO ()

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
