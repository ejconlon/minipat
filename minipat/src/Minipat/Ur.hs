module Minipat.Ur where

import Data.Map.Strict (Map)
import Minipat.Class (Pattern)
import Minipat.Time (CycleDelta)

ur
  :: (Pattern f, Ord k)
  => CycleDelta
  -> f k
  -> Map k (f a)
  -> Map k (f a -> f a)
  -> f a
ur = error "TODO"
