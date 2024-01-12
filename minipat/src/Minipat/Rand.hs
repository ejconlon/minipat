module Minipat.Rand where

import Data.Bits (Bits (..))
import Data.Ratio ((%))
import Data.Word (Word32)
import Minipat.Types (Arc, arcStart, timeFloor)

-- These random functions are more or less how Tidal does it:

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
arcSeed :: Arc -> Seed
arcSeed = timeSeed . fromInteger . timeFloor . arcStart

-- arcSeed = timeSeed . arcMid
-- TODO should be floor of arc start or just mid?

randFrac :: Seed -> Rational
randFrac (Seed s) = mod (fromIntegral s) seedConst % seedConst

randInt :: (Integral a) => a -> Seed -> a
randInt a s = floor (fromIntegral a * randFrac s)
