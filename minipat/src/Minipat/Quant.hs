module Minipat.Quant
  ( TimeStrat (..)
  , ArcStrat
  , quant
  , quantStep
  )
where

import Data.Ratio ((%))
import Minipat.Classes (Flow (..))
import Minipat.Stream (Stream, streamRun)
import Minipat.Tape (Ev (..), Tape)
import Minipat.Tape qualified as T
import Minipat.Time
  ( Arc (..)
  , CycleDelta (..)
  , CycleTime (..)
  , Measurable (..)
  , Span (..)
  , StepDelta (..)
  , StepTime (..)
  )

data TimeStrat
  = TimeStratRound
  | TimeStratCeiling
  | TimeStratFloor
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type ArcStrat = Arc TimeStrat

type StepFun = Integer -> CycleDelta -> StepDelta

type OutputFun a = CycleTime -> StepTime -> a

type AdjustFun a b c = OutputFun a -> Integer -> b -> c

adjustTime :: StepFun -> AdjustFun a CycleTime a
adjustTime stepFun outFun steps time =
  let cycOrigin = floor time
      stepOrigin = cycOrigin * steps
      cycDelta = measure (fromInteger cycOrigin) time
      stepDelta = stepFun steps cycDelta
      outCycDelta = CycleDelta (unStepDelta stepDelta % steps)
      outCycTime = shift outCycDelta (fromInteger cycOrigin)
      outStepTime = shift stepDelta (fromInteger stepOrigin)
  in  outFun outCycTime outStepTime

primStepFun :: (Rational -> Integer) -> StepFun
primStepFun f steps = StepDelta . f . (fromInteger steps *) . unCycleDelta

stratStepFun :: TimeStrat -> StepFun
stratStepFun =
  primStepFun . \case
    TimeStratRound -> round
    TimeStratCeiling -> ceiling
    TimeStratFloor -> floor

quantTime :: TimeStrat -> AdjustFun a CycleTime a
quantTime = adjustTime . stratStepFun

quantArc :: ArcStrat -> AdjustFun a (Arc CycleTime) (Arc a)
quantArc (Arc startStrat endStrat) outFun steps (Arc startTime endTime) =
  let f strat = quantTime strat outFun steps
  in  Arc (f startStrat startTime) (f endStrat endTime)

quantSpan :: ArcStrat -> AdjustFun a (Span CycleTime) (Span a)
quantSpan strat outFun steps (Span ac mwh) =
  let f = quantArc strat outFun steps
  in  Span (f ac) (fmap f mwh)

quantCycOut :: OutputFun CycleTime
quantCycOut x _ = x

quantStepOut :: OutputFun StepTime
quantStepOut _ y = y

-- | Quantize the pattern into the given number of steps per cycle by
-- nudging event start and end times to align with step times.
quant :: (Flow f) => ArcStrat -> Integer -> f a -> f a
quant strat = flowNudge . quantArc strat quantCycOut

-- | Quantize the stream and emit a tape of events over the given time range.
quantStep :: ArcStrat -> Integer -> Stream a -> Arc StepTime -> Tape StepTime a
quantStep strat steps stream stepArc = stepTape
 where
  cycArc = fmap (CycleTime . (% steps) . unStepTime) stepArc
  cycTape = streamRun stream cycArc
  onSpan = quantSpan strat quantStepOut steps
  stepTape = T.tapeConcatMap (\(Ev sp a) -> T.tapeSingleton (Ev (onSpan sp) a)) cycTape
