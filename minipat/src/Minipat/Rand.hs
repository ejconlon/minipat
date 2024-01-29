-- | "Randomness" in patterns is weaker than typical pseudo-randomness:
-- There is essentially one function from cycle time to random value that
-- we use for everything. (This is all pretty much how Tidal does it, specifically
-- the choice of xorshift moduli and period.)
module Minipat.Rand
  ( Seed
  , arcSeed
  , spanSeed
  , randFrac
  , randInt
  )
where

import Data.Bits (Bits (..))
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Word (Word32)
import Minipat.Time (Arc (..), Span (..), Time, arcMidpoint)

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

-- | Associates a random seed with a given 'Time'.
timeSeed :: Time -> Seed
timeSeed time =
  let (_, frac) = properFraction @_ @Word32 (time / 300)
      val = truncate (frac * seedConst)
  in  xorshift (Seed val)

-- | Associates a random seed with a given 'Arc'.
arcSeed :: Arc -> Seed
arcSeed = timeSeed . arcMidpoint

-- | Associates a random seed with a given 'Span'.
spanSeed :: Span -> Seed
spanSeed (Span arc mwhole) = arcSeed (fromMaybe arc mwhole)

-- | Returns a random fractional value in [0, 1)
randFrac :: Seed -> Rational
randFrac (Seed s) = mod (fromIntegral s) seedConst % seedConst

-- | Returns a random integral value in [0, n)
randInt :: (Integral a) => a -> Seed -> a
randInt a s = floor (fromIntegral a * randFrac s)
