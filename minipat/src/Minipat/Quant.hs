module Minipat.Quant
  ( TimeStrat (..)
  , ArcStrat
  , quant
  , Trig (..)
  , quantTrig
  )
where

import Bowtie (Anno (..))
import Data.Ratio ((%))
import Minipat.Classes (Flow (..))
import Minipat.Stream (Ev (..), Stream, streamChop)
import Minipat.Time (Arc (..), CycleArc, CycleSpan, CycleTime (..), Span (..), arcIntersect)

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
  , trigOff :: !Bool
  }
  deriving stock (Eq, Ord, Show)

quantTrig :: ArcStrat -> Integer -> Stream a -> Stream (Anno Trig a)
quantTrig strat steps = streamChop f
 where
  stepLen = 1 % steps
  addStep = CycleTime . (+ stepLen) . unCycleTime
  subStep = CycleTime . subtract stepLen . unCycleTime
  quantSteps = quantArc strat steps
  f (Ev (Span ac mwh) a) =
    case mwh of
      Nothing ->
        -- Signals have no on or off triggers
        let sp = Span (quantSteps ac) Nothing
            anno = Anno (Trig False False) a
        in  [Ev sp anno]
      Just wh ->
        let whq@(Arc s e) = quantSteps wh
            t = addStep s
            d = subStep e
        in  if t > d
              then
                -- Overlap
                let newWh = Arc s t
                    newAc = arcIntersect newWh whq
                    sp = Span newAc (Just newWh)
                    anno = Anno (Trig True True) a
                in  [Ev sp anno]
              else
                let newWh1 = Arc s t
                    newWh2 = Arc d e
                    newAc1 = arcIntersect newWh1 whq
                    newAc2 = arcIntersect newWh2 whq
                    sp1 = Span newAc1 (Just newWh1)
                    sp2 = Span newAc2 (Just newWh2)
                    anno1 = Anno (Trig True False) a
                    anno2 = Anno (Trig False True) a
                in  [Ev sp1 anno1, Ev sp2 anno2]
