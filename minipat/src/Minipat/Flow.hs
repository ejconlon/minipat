module Minipat.Flow where

import Control.Applicative (Alternative)
import Data.Sequence (Seq)
import Minipat.Pattern (Pattern)
import Minipat.Stream
  ( Stream
  , streamEarly
  , streamEarlyBy
  , streamFilter
  , streamLate
  , streamLateBy
  , streamPieces
  , streamSwitch
  )
import Minipat.Time (CycleDelta, CycleTime)

class (Alternative f, Pattern f) => Flow f where
  flowFilter :: (a -> Bool) -> f a -> f a
  flowEarlyBy, flowLateBy :: CycleDelta -> f a -> f a
  flowEarly, flowLate :: f CycleDelta -> f a -> f a
  flowSwitch :: f a -> CycleTime -> f a -> f a
  flowPieces :: f a -> Seq (CycleTime, f a) -> f a

instance Flow Stream where
  flowFilter = streamFilter
  flowEarlyBy = streamEarlyBy
  flowLateBy = streamLateBy
  flowEarly = streamEarly
  flowLate = streamLate
  flowSwitch = streamSwitch
  flowPieces = streamPieces
