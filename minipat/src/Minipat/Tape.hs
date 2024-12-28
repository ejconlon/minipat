module Minipat.Tape
  ( Ev (..)
  , Tape (..)
  , tapeNull
  , tapeFilter
  , tapeFastBy
  , tapeSlowBy
  , tapeEarlyBy
  , tapeLateBy
  , tapeDegradeBy
  , tapeTimeMapMono
  , tapeWholeMap
  , tapeUncons
  , tapeSingleton
  , tapeToList
  , tapeConcatMap
  , tapeFromList
  , tapeNudge
  , tapeHold
  )
where

import Data.Foldable (toList)
import Data.Heap (Entry (..), Heap)
import Data.Heap qualified as H
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Minipat.Rand (randFrac, spanSeed)
import Minipat.Time
  ( Arc (..)
  , CycleArc
  , CycleDelta (..)
  , CycleSpan
  , CycleTime (..)
  , Span (..)
  , arcIntersect
  , arcRelevant
  , spanMapWhole
  , spanNudge
  )
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as P

data Ev a = Ev
  { evSpan :: !CycleSpan
  , evValue :: !a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty a) => Pretty (Ev a) where
  pretty (Ev sp v) = P.hsep [pretty sp, pretty v]

-- evCont :: (CycleTime -> a) -> CycleArc -> Ev a
-- evCont f arc = Ev (Span arc Nothing) (f (arcStart arc))

newtype Tape a = Tape {unTape :: Heap (Entry CycleSpan a)}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Semigroup, Monoid)

instance Functor Tape where
  fmap f = Tape . H.mapMonotonic (\(Entry s a) -> Entry s (f a)) . unTape

tapeNull :: Tape a -> Bool
tapeNull = H.null . unTape

-- -- TODO Actually sample at the given rate
-- tapeCont :: Integer -> (CycleTime -> a) -> CycleArc -> Tape a
-- tapeCont _ f arc = tapeSingleton (evCont f arc)

tapeFilter :: (a -> Bool) -> Tape a -> Tape a
tapeFilter f = Tape . H.filter (\(Entry _ a) -> f a) . unTape

tapeFastBy :: Integer -> Rational -> Tape a -> Tape a
tapeFastBy o r =
  let o' = fromInteger o
  in  tapeTimeMapMono (\(CycleTime t) -> CycleTime ((t - o') / r + o'))

tapeSlowBy :: Integer -> Rational -> Tape a -> Tape a
tapeSlowBy o r =
  let o' = fromInteger o
  in  tapeTimeMapMono (\(CycleTime t) -> CycleTime ((t - o') * r + o'))

tapeLateBy :: CycleDelta -> Tape a -> Tape a
tapeLateBy (CycleDelta t) = tapeTimeMapMono (CycleTime . (+ t) . unCycleTime)

tapeEarlyBy :: CycleDelta -> Tape a -> Tape a
tapeEarlyBy (CycleDelta t) = tapeTimeMapMono (CycleTime . subtract t . unCycleTime)

tapeDegradeBy :: Rational -> Tape a -> Tape a
tapeDegradeBy r = Tape . H.filter f . unTape
 where
  f (Entry sp _) = randFrac (spanSeed sp) < r

tapeTimeMapMono :: (CycleTime -> CycleTime) -> Tape a -> Tape a
tapeTimeMapMono f = Tape . H.mapMonotonic (\(Entry s a) -> Entry (fmap f s) a) . unTape

tapeWholeMap :: (Maybe CycleArc -> Maybe CycleArc) -> Tape a -> Tape a
tapeWholeMap f = Tape . H.fromList . fmap (\(Entry s a) -> Entry (spanMapWhole f s) a) . toList . unTape

tapeSingleton :: Ev a -> Tape a
tapeSingleton (Ev s a) = Tape (H.singleton (Entry s a))

tapeUncons :: Tape a -> Maybe (Ev a, Tape a)
tapeUncons = fmap (\(Entry s a, h') -> (Ev s a, Tape h')) . H.uncons . unTape

tapeToList :: Tape a -> [Ev a]
tapeToList = fmap (\(Entry s a) -> Ev s a) . toList . unTape

tapeConcatMap :: (Ev a -> Tape b) -> Tape a -> Tape b
tapeConcatMap f = mconcat . fmap f . tapeToList

tapeFromList :: [Ev a] -> Tape a
tapeFromList = Tape . H.fromList . fmap (\(Ev s a) -> Entry s a)

-- Keep only relevant events (narrowing active arcs)
tapeRelevant :: CycleArc -> Tape a -> Tape a
tapeRelevant ref = Tape . H.fromList . mapMaybe go . toList . unTape
 where
  go (Entry s a) =
    if arcRelevant ref (spanActive s)
      then Just (Entry (s {spanActive = arcIntersect ref (spanActive s)}) a)
      else Nothing

tapeNudge :: (CycleArc -> CycleArc) -> CycleArc -> Tape a -> Tape a
tapeNudge f arc = Tape . H.mapMonotonic (\(Entry s a) -> Entry (spanNudge f arc s) a) . unTape

hold :: (Foldable f, Ord a, Eq b) => Arc a -> b -> f (Arc a, b) -> Seq (Arc a, b)
hold ac0@(Arc s0 e0) z0 = consolidate0 . toList
 where
  popEqStart z = \case
    hd :|> (Arc s' _, a) | a == z -> Just (hd, s')
    _ -> Nothing
  push s e z acc = case popEqStart z acc of
    Just (acc', s') -> acc' :|> (Arc s' e, z)
    Nothing -> acc :|> (Arc s e, z)
  consolidate0 es = case es of
    [] -> Seq.singleton (ac0, z0)
    (Arc s2 _, _) : _ ->
      if s2 > s0
        then consolidate (Seq.singleton (Arc s0 s2, z0)) s2 es
        else consolidate Empty s0 es
  consolidate !acc e1 = \case
    [] ->
      if e1 < e0
        then push e1 e0 z0 acc
        else acc
    (Arc _ e2, a) : es -> consolidate (push e1 e2 a acc) e2 es

tapeHold :: (Eq a) => CycleArc -> a -> Tape a -> Tape a
tapeHold ac0 z0 =
  Tape
    . H.fromList
    . fmap (\(ac, a) -> Entry (Span ac Nothing) a)
    . toList
    . hold ac0 z0
    . fmap (\(Entry (Span ac _) a) -> (ac, a))
    . toList
    . unTape
