{-# OPTIONS_GHC -fno-warn-identities #-}

-- | Time is a cube with four corners
module Minipat.Time
  ( midpoint
  , Measurable (..)
  , CycleTime (..)
  , CycleDelta (..)
  , Arc (..)
  , CycleArc
  , PosixArc
  , arcMidpoint
  , arcWiden
  , arcRelevant
  , arcUnion
  , arcIntersect
  , arcLength
  , MergeStrat (..)
  , arcMerge
  , Span (..)
  , CycleSpan
  , PosixSpan
  , spanCover
  , spanSplit
  , spanActiveStart
  , spanWholeLength
  , spanMapWhole
  , spanIsStart
  , bpmToCps
  , cpsToBpm
  , deltaToCycle
  , cycleToDelta
  , relativeDelta
  )
where

import Data.Maybe (fromMaybe)
import Minipat.Print (prettyRat, prettyTup)
import Nanotime (PosixTime, TimeDelta, diffTime, timeDeltaFromFracSecs, timeDeltaToFracSecs)
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as P

midpoint :: (Fractional a) => a -> a -> a
midpoint s e = s + (e - s) / 2

class Measurable b a | a -> b where
  -- | `measure start end` is `end - start`
  measure :: a -> a -> b

instance Measurable TimeDelta PosixTime where
  measure s e = diffTime e s

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

instance Measurable CycleDelta CycleTime where
  measure s e = CycleDelta (unCycleTime e - unCycleTime s)

data Arc a = Arc {arcStart :: !a, arcEnd :: !a}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type CycleArc = Arc CycleTime

type PosixArc = Arc PosixTime

instance (Pretty a) => Pretty (Arc a) where
  pretty (Arc s e) = prettyTup s e

arcWiden :: (RealFrac a) => Arc a -> Arc a
arcWiden (Arc s e) = Arc (fromInteger (floor s)) (fromInteger (ceiling e))

arcRelevant :: (Ord a) => Arc a -> Arc a -> Bool
arcRelevant (Arc s1 e1) (Arc s2 e2) = s2 < e1 && (e2 > s1 || (s2 == s1 && e2 == s1))

arcUnion :: (Ord a) => Arc a -> Arc a -> Arc a
arcUnion (Arc s1 e1) (Arc s2 e2) = Arc (min s1 s2) (max e1 e2)

arcIntersect :: (Ord a) => Arc a -> Arc a -> Arc a
arcIntersect (Arc s1 e1) (Arc s2 e2) =
  let s3 = max s1 s2
      e3 = min e1 e2
  in  Arc s3 (max s3 e3)

arcMidpoint :: (Fractional a) => Arc a -> a
arcMidpoint (Arc s e) = midpoint s e

-- | Strategy for merging arcs
data MergeStrat
  = MergeStratInner
  | MergeStratOuter
  | MergeStratMixed
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Merges arcs according to the given strategy
arcMerge :: (Ord a) => MergeStrat -> Maybe (Arc a) -> Maybe (Arc a) -> Maybe (Arc a)
arcMerge = \case
  MergeStratInner -> (\_ x -> x)
  MergeStratOuter -> const
  MergeStratMixed -> liftA2 arcIntersect

arcLength :: (Measurable b a) => Arc a -> b
arcLength (Arc s e) = measure s e

data Span a = Span
  { spanActive :: !(Arc a)
  , spanWhole :: !(Maybe (Arc a))
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type CycleSpan = Span CycleTime

type PosixSpan = Span PosixTime

instance (Pretty a) => Pretty (Span a) where
  pretty (Span ac wh) = P.hsep (pretty ac : maybe [] (pure . pretty) wh)

-- | Map a monotonic function over whole cycle times
spanMapWhole :: (Maybe (Arc a) -> Maybe (Arc a)) -> Span a -> Span a
spanMapWhole f (Span ac wh) = Span ac (f wh)

-- | Returns the 'Arc' covering the whole event
-- (or just the active arc if non-discrete)
spanCover :: Span a -> Arc a
spanCover (Span ac wh) = fromMaybe ac wh

-- | Splits an 'Arc' into single-cycle spans
spanSplit :: (RealFrac a) => Arc a -> [(Integer, Span a)]
spanSplit (Arc s0 e) =
  let ef = floor e
      go s =
        let sf = floor s
            si = fromInteger sf
            sc = fromInteger (floor s + 1)
            wh = Just (Arc si sc)
        in  if sf == ef || sc == e
              then [(sf, Span (Arc s e) wh)]
              else (sf, Span (Arc s sc) wh) : go sc
  in  go s0

-- | The start of the 'Span' in cycle time, if active
spanActiveStart :: (Eq a) => Span a -> Maybe a
spanActiveStart = \case
  sp@(Span _ (Just (Arc sw _))) | spanIsStart sp -> Just sw
  _ -> Nothing

-- | The length of the whole event in cycle time, if discrete
spanWholeLength :: (Measurable b a) => Span a -> Maybe b
spanWholeLength = \case
  Span _ (Just arc) -> Just (arcLength arc)
  _ -> Nothing

-- | True if active start aligns with whole start
spanIsStart :: (Eq a) => Span a -> Bool
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
relativeDelta :: Rational -> CycleTime -> CycleTime -> TimeDelta
relativeDelta cps start end = timeDeltaFromFracSecs (unCycleTime (end - start) / cps)
