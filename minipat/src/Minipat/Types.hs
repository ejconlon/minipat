module Minipat.Types where

import Control.Applicative (Alternative (..))
import Control.Monad (ap)
import Data.Foldable (toList)
import Data.Heap (Entry (..), Heap)
import Data.Heap qualified as H

type Time = Rational

timeFloor :: Time -> Integer
timeFloor = floor

timeCeil :: Time -> Integer
timeCeil = (+ 1) . timeFloor

timeLerp :: Time -> Time -> Time
timeLerp s e = s + (e - s) / 2

data Arc = Arc {arcStart :: !Time, arcEnd :: !Time}
  deriving stock (Eq, Ord, Show)

arcUnion :: Arc -> Arc -> Arc
arcUnion (Arc s1 e1) (Arc s2 e2) = Arc (min s1 s2) (max e1 e2)

arcIntersect :: Arc -> Arc -> Arc
arcIntersect (Arc s1 e1) (Arc s2 e2) =
  let s3 = max s1 s2
      e3 = min e1 e2
  in  Arc s3 (max s3 e3)

arcWiden :: Arc -> Arc
arcWiden (Arc s e) = Arc (fromInteger (timeFloor s)) (fromInteger (timeCeil e))

arcMid :: Arc -> Time
arcMid (Arc s e) = timeLerp s e

arcTimeMapMono :: (Time -> Time) -> Arc -> Arc
arcTimeMapMono f (Arc s e) = Arc (f s) (f e)

data Span = Span
  { spanActive :: !Arc
  , spanWhole :: !(Maybe Arc)
  }
  deriving stock (Eq, Ord, Show)

spanTimeMapMono :: (Time -> Time) -> Span -> Span
spanTimeMapMono f (Span ac wh) = Span (arcTimeMapMono f ac) (fmap (arcTimeMapMono f) wh)

spanWholeMapMono :: (Maybe Arc -> Maybe Arc) -> Span -> Span
spanWholeMapMono f (Span ac wh) = Span ac (f wh)

spanSplit :: Arc -> [Span]
spanSplit (Arc s0 e) =
  let ef = fromInteger (timeFloor e)
      go s =
        let sf = fromInteger (timeFloor s)
            sc = fromInteger (timeCeil s)
            wh = Just (Arc sf sc)
        in  if sf == ef || sc == e
              then [Span (Arc s e) wh]
              else Span (Arc s sc) wh : go sc
  in  go s0

data Ev a = Ev
  { evSpan :: !Span
  , evValue :: !a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

evCont :: (Time -> a) -> Arc -> Ev a
evCont f arc = Ev (Span arc Nothing) (f (arcMid arc))

-- evLte :: Ev a -> Ev a -> Bool
-- evLte (Ev p1 _) (Ev p2 _) = p1 <= p2
--
-- evMerge :: [Ev a] -> [Ev a] -> [Ev a]
-- evMerge [] rs = rs
-- evMerge ls [] = ls
-- evMerge ls@(l:ls') rs@(r:rs') =
--   if evLte l r
--     then l : evMerge ls' rs
--     else r : evMerge ls rs'

newtype Tape a = Tape {unTape :: Heap (Entry Span a)}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Semigroup, Monoid)

instance Functor Tape where
  fmap f = Tape . H.mapMonotonic (\(Entry s a) -> Entry s (f a)) . unTape

tapeTimeMapMono :: (Time -> Time) -> Tape a -> Tape a
tapeTimeMapMono f = Tape . H.mapMonotonic (\(Entry s a) -> Entry (spanTimeMapMono f s) a) . unTape

tapeWholeMapMono :: (Maybe Arc -> Maybe Arc) -> Tape a -> Tape a
tapeWholeMapMono f = Tape . H.mapMonotonic (\(Entry s a) -> Entry (spanWholeMapMono f s) a) . unTape

tapeSingle :: Ev a -> Tape a
tapeSingle (Ev s a) = Tape (H.singleton (Entry s a))

tapeUncons :: Tape a -> Maybe (Ev a, Tape a)
tapeUncons = fmap (\(Entry s a, h') -> (Ev s a, Tape h')) . H.uncons . unTape

tapeToList :: Tape a -> [Ev a]
tapeToList = fmap (\(Entry s a) -> Ev s a) . toList . unTape

tapeConcatMap :: (Ev a -> Tape b) -> Tape a -> Tape b
tapeConcatMap f = mconcat . fmap f . tapeToList

newtype Pat a = Pat {unPat :: Arc -> Tape a}

instance Functor Pat where
  fmap f (Pat k) = Pat (fmap f . k)

instance Applicative Pat where
  pure = patImpulse
  (<*>) = ap

instance Monad Pat where
  (>>=) = patMixBind

instance Alternative Pat where
  empty = Pat (const (Tape H.empty))
  Pat k1 <|> Pat k2 = Pat (\arc -> k1 arc <> k2 arc)

instance Semigroup (Pat a) where
  (<>) = (<|>)

instance Monoid (Pat a) where
  mempty = empty

patTimeMapInv :: (Time -> Time) -> (Time -> Time) -> Pat a -> Pat a
patTimeMapInv f g (Pat k) = Pat (tapeTimeMapMono f . k . arcTimeMapMono g)

patFastBy, patSlowBy :: Rational -> Pat a -> Pat a
patFastBy t = patTimeMapInv (/ t) (* t)
patSlowBy t = patTimeMapInv (+ t) (/ t)

patEarlyBy, patLateBy :: Time -> Pat a -> Pat a
patEarlyBy t = patTimeMapInv (+ t) (subtract t)
patLateBy t = patTimeMapInv (subtract t) (+ t)

patImpulse :: a -> Pat a
patImpulse a = Pat (Tape . H.fromList . fmap (`Entry` a) . spanSplit)

patBindWith :: (Maybe Arc -> Maybe Arc -> Maybe Arc) -> Pat a -> (a -> Pat b) -> Pat b
patBindWith g pa f = Pat $ \arc ->
  let ta = unPat pa arc
  in  flip tapeConcatMap ta $ \(Ev (Span ac wh) a) ->
        let tb = unPat (f a) ac
        in  tapeWholeMapMono (g wh) tb

patInnerBind :: Pat a -> (a -> Pat b) -> Pat b
patInnerBind = patBindWith (\_ x -> x)

patOuterBind :: Pat a -> (a -> Pat b) -> Pat b
patOuterBind = patBindWith const

patMixBind :: Pat a -> (a -> Pat b) -> Pat b
patMixBind = patBindWith (liftA2 arcIntersect)

patAdjust :: (a -> Pat b -> Pat c) -> Pat a -> Pat b -> Pat c
patAdjust f pa pb = patInnerBind pa (`f` pb)

patFast, patSlow :: Pat Rational -> Pat a -> Pat a
patFast = patAdjust patFastBy
patSlow = patAdjust patSlowBy

patEarly, patLate :: Pat Time -> Pat a -> Pat a
patEarly = patAdjust patEarlyBy
patLate = patAdjust patLateBy

patCont :: (Time -> a) -> Pat a
patCont f = Pat (tapeSingle . evCont f)

fnSine :: Rational -> Time -> Double
fnSine freq t = sin (2 * pi * fromRational (freq * t))

patSine :: Rational -> Pat Double
patSine = patCont . fnSine

patRun :: Pat a -> Arc -> [Ev a]
patRun pa arc = tapeToList (unPat pa arc)
