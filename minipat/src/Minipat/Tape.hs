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
  , Measurable (..)
  , Span (..)
  , arcIntersect
  , arcRelevant
  , scale
  , spanMapWhole
  , spanNudge
  )
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as P

data Ev t a = Ev
  { evSpan :: !(Span t)
  , evValue :: !a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty t, Pretty a) => Pretty (Ev t a) where
  pretty (Ev sp v) = P.hsep [pretty sp, pretty v]

newtype Tape t a = Tape {unTape :: Heap (Entry (Span t) a)}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Semigroup, Monoid)

instance (Ord t) => Functor (Tape t) where
  fmap f = Tape . H.mapMonotonic (\(Entry s a) -> Entry s (f a)) . unTape

tapeNull :: Tape t a -> Bool
tapeNull = H.null . unTape

tapeFilter :: (a -> Bool) -> Tape t a -> Tape t a
tapeFilter f = Tape . H.filter (\(Entry _ a) -> f a) . unTape

tapeFastBy :: (RealFrac d, Measurable d t) => t -> Rational -> Tape t a -> Tape t a
tapeFastBy o = tapeTimeMapMono . scale o . recip

tapeSlowBy :: (RealFrac d, Measurable d t) => t -> Rational -> Tape t a -> Tape t a
tapeSlowBy o = tapeTimeMapMono . scale o

tapeLateBy :: (Measurable d t) => d -> Tape t a -> Tape t a
tapeLateBy = tapeTimeMapMono . shift

tapeEarlyBy :: (Measurable d t) => d -> Tape t a -> Tape t a
tapeEarlyBy = tapeTimeMapMono . shift . negate

tapeDegradeBy :: (RealFrac t, Fractional d, Measurable d t) => Rational -> Tape t a -> Tape t a
tapeDegradeBy r = Tape . H.filter f . unTape
 where
  f (Entry sp _) = randFrac (spanSeed sp) < r

tapeTimeMapMono :: (Ord t) => (t -> t) -> Tape t a -> Tape t a
tapeTimeMapMono f = Tape . H.mapMonotonic (\(Entry s a) -> Entry (fmap f s) a) . unTape

tapeWholeMap :: (Ord t) => (Maybe (Arc t) -> Maybe (Arc t)) -> Tape t a -> Tape t a
tapeWholeMap f = Tape . H.fromList . fmap (\(Entry s a) -> Entry (spanMapWhole f s) a) . toList . unTape

tapeSingleton :: (Ord t) => Ev t a -> Tape t a
tapeSingleton (Ev s a) = Tape (H.singleton (Entry s a))

tapeUncons :: Tape t a -> Maybe (Ev t a, Tape t a)
tapeUncons = fmap (\(Entry s a, h') -> (Ev s a, Tape h')) . H.uncons . unTape

tapeToList :: Tape t a -> [Ev t a]
tapeToList = fmap (\(Entry s a) -> Ev s a) . toList . unTape

tapeConcatMap :: (Ev t a -> Tape u b) -> Tape t a -> Tape u b
tapeConcatMap f = mconcat . fmap f . tapeToList

tapeFromList :: (Ord t) => [Ev t a] -> Tape t a
tapeFromList = Tape . H.fromList . fmap (\(Ev s a) -> Entry s a)

-- Keep only relevant events (narrowing active arcs)
tapeRelevant :: (Ord t) => Arc t -> Tape t a -> Tape t a
tapeRelevant ref = Tape . H.fromList . mapMaybe go . toList . unTape
 where
  go (Entry s a) =
    if arcRelevant ref (spanActive s)
      then Just (Entry (s {spanActive = arcIntersect ref (spanActive s)}) a)
      else Nothing

tapeNudge :: (Ord t) => (Arc t -> Arc t) -> Arc t -> Tape t a -> Tape t a
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

tapeHold :: (Ord t, Eq a) => Arc t -> a -> Tape t a -> Tape t a
tapeHold ac0 z0 =
  Tape
    . H.fromList
    . fmap (\(ac, a) -> Entry (Span ac Nothing) a)
    . toList
    . hold ac0 z0
    . fmap (\(Entry (Span ac _) a) -> (ac, a))
    . toList
    . unTape
