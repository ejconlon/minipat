module Minipat.Rand
  ( Seed
  , arcSeed
  , randFrac
  , randInt
  )
where

import Data.Bits (Bits (..))
import Data.Ratio ((%))
import Data.Word (Word32)
import Minipat.Base (Arc, arcStart, timeFloor)

-- These random functions are more or less how Tidal does it:

-- | A random seed
newtype Seed = Seed {unSeed :: Word32}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

seedConst :: (Num a) => a
seedConst = 536870912

-- A "random" number generator
-- See https://en.wikipedia.org/wiki/Xorshift
xorshift :: Seed -> Seed
xorshift (Seed x0) =
  let x1 = xor x0 (shiftL x0 13)
      x2 = xor x1 (shiftR x1 17)
  in  Seed (xor x2 (shiftL x2 5))

-- | Associates a "random" seed with a given "time".
timeSeed :: Rational -> Seed
timeSeed time =
  let (_, frac) = properFraction @_ @Word32 (time / 300)
      val = truncate (frac * seedConst)
  in  xorshift (Seed val)

-- | Associates a "random" seed with a given "arc".
-- TODO should be floor of arc start or just mid like:
-- arcSeed = timeSeed . arcMid
-- Choosing start makes more sense to me but it's not
-- how Tidal does it IIRC
arcSeed :: Arc -> Seed
arcSeed = timeSeed . fromInteger . timeFloor . arcStart

-- | Returns a random fractional value in [0, 1)
randFrac :: Seed -> Rational
randFrac (Seed s) = mod (fromIntegral s) (seedConst - 1) % seedConst

-- | Returns a random integral value in [0, n)
randInt :: (Integral a) => a -> Seed -> a
randInt a s = floor (fromIntegral a * randFrac s)
