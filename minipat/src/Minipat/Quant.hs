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

type OriginFun = CycleTime -> CycleTime

type OutputFun a = CycleTime -> CycleDelta -> StepTime -> StepDelta -> a

type AdjustFun a b c = OriginFun -> OutputFun a -> Integer -> b -> c

adjustTime :: StepFun -> AdjustFun a CycleTime a
adjustTime stepFun originFun outFun steps time =
  let cycOrigin = originFun time
      stepOrigin = floor (cycOrigin * fromInteger steps)
      cycDelta = measure cycOrigin time
      stepDelta = stepFun steps cycDelta
      newCycDelta = CycleDelta (unStepDelta stepDelta % steps)
  in  outFun cycOrigin newCycDelta stepOrigin stepDelta

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
quantArc (Arc startStrat endStrat) originFun outFun steps (Arc startTime endTime) =
  let f strat = quantTime strat originFun outFun steps
  in  Arc (f startStrat startTime) (f endStrat endTime)

quantSpan :: ArcStrat -> AdjustFun a (Span CycleTime) (Span a)
quantSpan strat originFun outFun steps (Span ac mwh) =
  let f = quantArc strat originFun outFun steps
  in  Span (f ac) (fmap f mwh)

quantOrigin :: OriginFun
quantOrigin = CycleTime . fromInteger . floor . unCycleTime

quantCycOut :: OutputFun CycleTime
quantCycOut o d _ _ = shift d o

quantStepOut :: OutputFun StepTime
quantStepOut _ _ o d = shift d o

-- | Quantize the pattern into the given number of steps per cycle by
-- nudging event start and end times to align with step times.
quant :: (Flow f) => ArcStrat -> Integer -> f a -> f a
quant strat = flowNudge . quantArc strat quantOrigin quantCycOut

-- | Quantize the stream and emit a tape of events over the given time range.
quantStep :: ArcStrat -> Integer -> Stream a -> Arc StepTime -> Tape StepTime a
quantStep strat steps stream stepArc = stepTape
 where
  stepLen = CycleTime (1 % steps)
  cycArc = fmap ((stepLen *) . fromIntegral) stepArc
  cycTape = streamRun stream cycArc
  onSpan = quantSpan strat quantOrigin quantStepOut steps
  stepTape = T.tapeConcatMap (\(Ev sp a) -> T.tapeSingleton (Ev (onSpan sp) a)) cycTape
