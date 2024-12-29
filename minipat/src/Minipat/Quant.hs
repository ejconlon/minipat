module Minipat.Quant
  ( TimeStrat (..)
  , ArcStrat
  , quant
  -- , Trig (..)
  , Step (..)
  , StepArc
  , StepSpan
  -- , quantTrig
  -- , Block (..)
  -- , quantBlock
  )
where

import Bowtie (Anno (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Ratio ((%))
import Data.Set (Set)
import Data.Set qualified as Set
import Minipat.Classes (Flow (..))
import Minipat.Stream (Stream, streamRun)
import Minipat.Tape (Ev (..), Tape)
import Minipat.Tape qualified as T
import Minipat.Time (Arc (..), CycleArc, CycleDelta (..), CycleSpan, CycleTime (..), Span (..), arcIntersect)

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

newtype Step = Step {unStep :: Integer}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)

type StepArc = Arc Step

type StepSpan = Span Step

-- data Trig = Trig
--   { trigOn :: !Bool
--   , trigOff :: !Bool
--   }
--   deriving stock (Eq, Ord, Show)

-- quantTrigChop :: ArcStrat -> Integer -> Ev a -> Tape (Anno StepSpan a)
-- quantTrigChop strat steps = chop
--  where
--   stepLen = 1 % steps
--   addStep = CycleTime . (+ stepLen) . unCycleTime
--   subStep = CycleTime . subtract stepLen . unCycleTime
--   quantSteps = quantArc strat steps
--   chop (Ev (Span ac mwh) a) =
--     case mwh of
--       Nothing ->
--         -- Signals have no on or off triggers
--         let newAc = quantSteps ac
--             sp = Span newAc Nothing
--             anno = Anno (Trig False False) a
--         in  T.tapeSingleton (Ev sp anno)
--       Just wh ->
--         let whq@(Arc s e) = quantSteps wh
--             t = addStep s
--             d = subStep e
--         in  if t > d
--               then
--                 -- Overlap
--                 let newWh = Arc s t
--                     newAc = arcIntersect newWh whq
--                     sp = Span newAc (Just newWh)
--                     anno = Anno (Trig True True) a
--                 in  T.tapeSingleton (Ev sp anno)
--               else
--                 let newWh1 = Arc s t
--                     newWh2 = Arc d e
--                     newAc1 = arcIntersect newWh1 whq
--                     newAc2 = arcIntersect newWh2 whq
--                     sp1 = Span newAc1 (Just newWh1)
--                     sp2 = Span newAc2 (Just newWh2)
--                     anno1 = Anno (Trig True False) a
--                     anno2 = Anno (Trig False True) a
--                 in  T.tapeFromList [Ev sp1 anno1, Ev sp2 anno2]
--
-- quantTrig :: (Flow f) => ArcStrat -> Integer -> f a -> f (Anno StepSpan a)
-- quantTrig strat steps = flowChop (quantTrigChop strat steps)
--
-- data Block a = Block
--   { blockSteps :: !Integer
--   , blockEvs :: !(IntMap a)
--   }
--   deriving stock (Eq, Ord, Show)
--
-- blockStepLen :: Block a -> CycleDelta
-- blockStepLen (Block steps _) = CycleDelta (1 % steps)
--
-- blockStepDelta :: Block a -> Step -> CycleDelta
-- blockStepDelta (Block steps _) (Step step) = CycleDelta (step % steps)
--
-- blockIter :: Block a -> [Anno Step a]
-- blockIter block = res
--  where
--   res = foldr go [] (IntMap.toList (blockEvs block))
--   go (i, a) ts = let j = fromIntegral i in Anno j a : ts

-- quantBlock :: (Ord a) => ArcStrat -> Integer -> Stream a -> CycleArc -> Block (Set (Anno Trig a))
-- quantBlock strat steps str arc = res
--  where
--   ixStep = floor . (steps % 1 *) . unCycleTime . arcStart . spanActive
--   newStr = quantTrig strat steps str
--   evTape = streamRun newStr arc
--   evMap = foldr insertEv IntMap.empty (T.tapeToList evTape)
--   insertEv (Ev sp x) = IntMap.insertWith (<>) (ixStep sp) (Set.singleton x)
--   res = Block steps evMap

-- data MonoTrig = MonoTrigOn | MonoTrigOff deriving stock (Eq, Ord, Show, Enum, Bounded)
--
-- best :: Set (Anno Trig a) -> Maybe (Anno MonoTrig a, Bool)
-- best = go Nothing . Set.toList where
--   go !acc = \case
--     [] -> Nothing
--     x@(Anno t _) : xs -> case t of
--       Trig True False -> Just x
--       Trig True True -> case acc of
--         Nothing -> go (Just x) xs
--         _ -> undefined
--       Trig False True -> case acc of
--         Nothing -> go (Just x) xs
--
--       Trig False False -> case acc of
--         Nothing -> go (Just x) xs
--         _ -> undefined
--
-- -- In order, prefer [ON, ON/OFF, OFF], pushing offs to the next step.
-- blockMono :: Block (Set (Anno Trig a)) -> Block (Anno MonoTrig a)
-- blockMono = undefined
