module Minipat.Class where

import Data.Sequence.NonEmpty (NESeq)
import Minipat.Time (CycleDelta)

-- | 'Pat' and 'Stream' can be constructed abstractly with this
class (Functor f) => Pattern f where
  patPure :: a -> f a
  patEmpty :: f a
  patPar :: NESeq (f a) -> f a
  patAlt :: NESeq (f a) -> f a
  patRand :: NESeq (f a) -> f a
  patSeq :: NESeq (f a, CycleDelta) -> f a
  patEuc :: Int -> Int -> Maybe Int -> f a -> f a
  patRep :: Int -> f a -> f a
  patFastBy, patSlowBy :: Rational -> f a -> f a
  patFast, patSlow :: f Rational -> f a -> f a
  patDegBy :: Rational -> f a -> f a
  patDeg :: f Rational -> f a -> f a
