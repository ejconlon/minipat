{-# OPTIONS_GHC -fno-warn-identities #-}

-- | Time is a cube with four corners
module Minipat.Time
  ( CycleTime (..)
  , CycleDelta (..)
  , Cycle (..)
  , cycTimeFloor
  , cycTimeCeil
  , cycTimeMid
  , Arc (..)
  , arcWiden
  , arcRelevant
  , arcUnion
  , arcIntersect
  , arcMid
  , arcTimeMapMono
  , MergeStrat (..)
  , arcMerge
  , Span (..)
  , spanCover
  , spanSplit
  , spanCycle
  , spanDelta
  , spanTimeMapMono
  , spanWholeMapMono
  , spanIsStart
  , bpmToCps
  , cpsToBpm
  , deltaToCycle
  , cycleToDelta
  , relDelta
  )
where

import Data.Maybe (fromMaybe)
import Minipat.Print (prettyRat, prettyTup)
import Nanotime (TimeDelta, timeDeltaFromFracSecs, timeDeltaToFracSecs)
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as P

newtype CycleTime = CycleTime {unCycleTime :: Rational}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Fractional, Real, RealFrac)

instance Pretty CycleTime where
  pretty = prettyRat . unCycleTime

newtype CycleDelta = CycleDelta {unCycleDelta :: Rational}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Fractional, Real, RealFrac)

instance Pretty CycleDelta where
  pretty = prettyRat . unCycleDelta

newtype Cycle = Cycle {unCycle :: Integer}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Pretty)

cycTimeFloor :: CycleTime -> Cycle
cycTimeFloor = Cycle . floor . unCycleTime

cycTimeCeil :: CycleTime -> Cycle
cycTimeCeil = (+ 1) . cycTimeFloor

cycTimeMid :: CycleTime -> CycleTime -> CycleTime
cycTimeMid s e = s + (e - s) / 2

data Arc = Arc {arcStart :: !CycleTime, arcEnd :: !CycleTime}
  deriving stock (Eq, Ord, Show)

instance Pretty Arc where
  pretty (Arc s e) = prettyTup s e

arcWiden :: Arc -> Arc
arcWiden (Arc s e) = Arc (fromInteger (floor (unCycleTime s))) (fromInteger (floor (unCycleTime e) + 1))

arcRelevant :: Arc -> Arc -> Bool
arcRelevant (Arc s1 e1) (Arc s2 e2) = s2 < e1 && (e2 > s1 || (s2 == s1 && e2 == s1))

arcUnion :: Arc -> Arc -> Arc
arcUnion (Arc s1 e1) (Arc s2 e2) = Arc (min s1 s2) (max e1 e2)

arcIntersect :: Arc -> Arc -> Arc
arcIntersect (Arc s1 e1) (Arc s2 e2) =
  let s3 = max s1 s2
      e3 = min e1 e2
  in  Arc s3 (max s3 e3)

arcMid :: Arc -> CycleTime
arcMid (Arc s e) = cycTimeMid s e

-- | Map a monotonic function over cycle times
arcTimeMapMono :: (CycleTime -> CycleTime) -> Arc -> Arc
arcTimeMapMono f (Arc s e) = Arc (f s) (f e)

-- | Strategy for merging arcs
data MergeStrat
  = MergeStratInner
  | MergeStratOuter
  | MergeStratMixed
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Merges arcs according to the given strategy
arcMerge :: MergeStrat -> Maybe Arc -> Maybe Arc -> Maybe Arc
arcMerge = \case
  MergeStratInner -> (\_ x -> x)
  MergeStratOuter -> const
  MergeStratMixed -> liftA2 arcIntersect

data Span = Span
  { spanActive :: !Arc
  , spanWhole :: !(Maybe Arc)
  }
  deriving stock (Eq, Ord, Show)

instance Pretty Span where
  pretty (Span ac wh) = P.hsep (pretty ac : maybe [] (pure . pretty) wh)

-- | Map a monotonic function over all cycle times
spanTimeMapMono :: (CycleTime -> CycleTime) -> Span -> Span
spanTimeMapMono f (Span ac wh) = Span (arcTimeMapMono f ac) (fmap (arcTimeMapMono f) wh)

-- | Map a monotonic function over whole cycle times
spanWholeMapMono :: (Maybe Arc -> Maybe Arc) -> Span -> Span
spanWholeMapMono f (Span ac wh) = Span ac (f wh)

-- | Returns the 'Arc' covering the whole event
-- (or just the active arc if non-discrete)
spanCover :: Span -> Arc
spanCover (Span ac wh) = fromMaybe ac wh

-- | Splits an 'Arc' into single-cycle spans
spanSplit :: Arc -> [(Cycle, Span)]
spanSplit (Arc s0 e) =
  let ef = cycTimeFloor e
      go s =
        let sf = cycTimeFloor s
            si = fromInteger (unCycle sf)
            sc = fromInteger (unCycle (cycTimeCeil s))
            wh = Just (Arc si sc)
        in  if sf == ef || sc == e
              then [(sf, Span (Arc s e) wh)]
              else (sf, Span (Arc s sc) wh) : go sc
  in  go s0

-- | The start of the 'Span' in cycle time, if active
spanCycle :: Span -> Maybe CycleTime
spanCycle = \case
  sp@(Span _ (Just (Arc sw _))) | spanIsStart sp -> Just sw
  _ -> Nothing

-- | The length of the whole event in cycle time, if discrete
spanDelta :: Span -> Maybe CycleDelta
spanDelta = \case
  Span _ (Just (Arc sw ew)) -> Just (CycleDelta (unCycleTime (ew - sw)))
  _ -> Nothing

-- | True if active start aligns with whole start
spanIsStart :: Span -> Bool
spanIsStart (Span (Arc sa _) mwh) =
  case mwh of
    Nothing -> True
    Just (Arc sw _) -> sa == sw

-- | Convert BPM to CPS, given BPC (beats per cycle/bar)
-- (often 4 beats/bar, 1 bar/cycle, so 4 bpc)
bpmToCps :: Integer -> Rational -> Rational
bpmToCps bpc bpm = (bpm / 60) / fromInteger bpc

-- | Convert CPS to BPM, given BPC (beats per cycle/bar)
cpsToBpm :: Integer -> Rational -> Rational
cpsToBpm bpc cps = (cps * fromInteger bpc) * 60

-- | Given CPS convert absolute time diff from start to cycle time
deltaToCycle :: Rational -> TimeDelta -> CycleTime
deltaToCycle cps = CycleTime . (cps *) . timeDeltaToFracSecs

-- | Given CPS convert cycle time to absolute time diff from start
cycleToDelta :: Rational -> CycleTime -> TimeDelta
cycleToDelta cps = timeDeltaFromFracSecs . (/ cps) . unCycleTime

-- | Given CPS return relative time from origin to target
relDelta :: Rational -> CycleTime -> CycleTime -> TimeDelta
relDelta cps origin target = timeDeltaFromFracSecs (unCycleTime (target - origin) / cps)
