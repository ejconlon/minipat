module Minipat.Quant
  ( TimeStrat (..)
  , ArcStrat
  , quant
  )
where

import Bowtie (Anno (..))
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Minipat.Classes (Flow (..))
import Minipat.Stream (Ev (..), Stream, streamChop)
import Minipat.Time (Arc (..), CycleArc, CycleDelta (..), CycleSpan, CycleTime (..), Span (..))

data TimeStrat
  = TimeStratRound
  | TimeStratCeiling
  | TimeStratFloor
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type ArcStrat = Arc TimeStrat

adjustTime :: (Rational -> Integer) -> Integer -> CycleTime -> CycleTime
adjustTime f steps (CycleTime time) =
  let cyc = fromInteger (floor time)
      off = time - cyc
      off' = f (off * fromInteger steps) % steps
  in  CycleTime (cyc + off')

roundTime :: Integer -> CycleTime -> CycleTime
roundTime = adjustTime round

ceilingTime :: Integer -> CycleTime -> CycleTime
ceilingTime = adjustTime ceiling

floorTime :: Integer -> CycleTime -> CycleTime
floorTime = adjustTime floor

quantTime :: TimeStrat -> Integer -> CycleTime -> CycleTime
quantTime = \case
  TimeStratRound -> roundTime
  TimeStratCeiling -> ceilingTime
  TimeStratFloor -> floorTime

quantArc :: ArcStrat -> Integer -> CycleArc -> CycleArc
quantArc (Arc startStrat endStrat) steps (Arc startTime endTime) =
  Arc (quantTime startStrat steps startTime) (quantTime endStrat steps endTime)

quantSpan :: ArcStrat -> Integer -> CycleSpan -> CycleSpan
quantSpan strat steps (Span ac mwh) =
  let f = quantArc strat steps
  in  Span (f ac) (fmap f mwh)

-- | Quantize the pattern into the given number of steps per cycle by
-- nudging event start and end times to align with step times.
quant :: (Flow f) => ArcStrat -> Integer -> f a -> f a
quant strat steps = flowNudge (quantArc strat steps)

data Trig = Trig
  { trigOn :: !Bool
  , trigOff :: !(Maybe CycleDelta)
  }
  deriving stock (Eq, Ord, Show)

-- quantOnOff :: ArcStrat -> Integer -> Stream a -> Stream (Anno Trig a)
-- quantOnOff strat steps = streamChop f where
--   r = 1 % steps
--   h = quantArc strat steps
--   g = CycleTime . subtract r . unCycleTime
--   f (Ev (Span ac mwh) a) =
--     case mwh of
--       -- Signals have no on or off triggers
--       Nothing -> [Ev (Span (h ac) Nothing) (Anno (Trig False Nothing) a)]
--       Just wh ->
--         let whq@(Arc s e) = h wh
--             d = g e
--         in if d <= s
--           then error "TODO"
--           else error "TODO"
--           -- then [Ev (pan (h ac) (Just whq)) (Anno (Trig True (Just (CycleDelta r))) a)]
--           -- else _
