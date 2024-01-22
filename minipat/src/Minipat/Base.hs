module Minipat.Base
  ( Time
  , timeFloor
  , timeCeil
  , timeLerp
  , Arc (..)
  , arcUnion
  , arcIntersect
  , arcWiden
  , arcMid
  , Span (..)
  , spanSplit
  , Ev (..)
  , evCont
  , Tape
  , Pat (..)
  , patInnerBind
  , patOuterBind
  , patMixBind
  , patRun
  , patAdjust
  , patConcat
  , patFastBy
  , patSlowBy
  , patFast
  , patSlow
  , patEarlyBy
  , patLateBy
  , patEarly
  , patLate
  , patDegradeBy
  , patDegrade
  , patCont
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (ap)
import Data.Foldable (foldl', toList)
import Data.Foldable1 (foldMap1')
import Data.Heap (Entry (..), Heap)
import Data.Heap qualified as H
import Data.Semigroup (Sum (..))
import Data.Sequence.NonEmpty (NESeq)
import Data.String (IsString (..))

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

arcWrap :: Arc -> Rational -> (Time, Time, Time)
arcWrap (Arc s e) w =
  if s >= w
    then
      let d = e - s
          k = w * fromInteger (floor (s / w))
          c = s - k
      in  (k, c, d)
    else (0, s, e - s)

data Span = Span
  { spanActive :: !Arc
  , spanWhole :: !(Maybe Arc)
  }
  deriving stock (Eq, Ord, Show)

spanTimeMapMono :: (Time -> Time) -> Span -> Span
spanTimeMapMono f (Span ac wh) = Span (arcTimeMapMono f ac) (fmap (arcTimeMapMono f) wh)

spanWholeMapMono :: (Maybe Arc -> Maybe Arc) -> Span -> Span
spanWholeMapMono f (Span ac wh) = Span ac (f wh)

spanSplit :: Arc -> [(Integer, Span)]
spanSplit (Arc s0 e) =
  let ef = fromInteger (timeFloor e)
      go s =
        let si = timeFloor s
            sf = fromInteger si
            sc = fromInteger (timeCeil s)
            wh = Just (Arc sf sc)
        in  if sf == ef || sc == e
              then [(si, Span (Arc s e) wh)]
              else (si, Span (Arc s sc) wh) : go sc
  in  go s0

data Ev a = Ev
  { evSpan :: !Span
  , evValue :: !a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

evCont :: (Time -> a) -> Arc -> Ev a
evCont f arc = Ev (Span arc Nothing) (f (arcStart arc))

newtype Tape a = Tape {unTape :: Heap (Entry Span a)}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Semigroup, Monoid)

instance Functor Tape where
  fmap f = Tape . H.mapMonotonic (\(Entry s a) -> Entry s (f a)) . unTape

tapeFastBy :: Integer -> Rational -> Tape a -> Tape a
tapeFastBy o r =
  let o' = fromInteger o
  in  tapeTimeMapMono (\t -> (t - o') / r + o')

tapeSlowBy :: Integer -> Rational -> Tape a -> Tape a
tapeSlowBy o r =
  let o' = fromInteger o
  in  tapeTimeMapMono (\t -> (t - o') * r + o')

tapeLateBy :: Time -> Tape a -> Tape a
tapeLateBy t = tapeTimeMapMono (+ t)

tapeEarlyBy :: Time -> Tape a -> Tape a
tapeEarlyBy t = tapeTimeMapMono (subtract t)

tapeTimeMapMono :: (Time -> Time) -> Tape a -> Tape a
tapeTimeMapMono f = Tape . H.mapMonotonic (\(Entry s a) -> Entry (spanTimeMapMono f s) a) . unTape

tapeWholeMapMono :: (Maybe Arc -> Maybe Arc) -> Tape a -> Tape a
tapeWholeMapMono f = Tape . H.mapMonotonic (\(Entry s a) -> Entry (spanWholeMapMono f s) a) . unTape

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

newtype Pat a = Pat {unPat :: Arc -> Tape a}

instance Functor Pat where
  fmap f (Pat k) = Pat (fmap f . k)

instance Applicative Pat where
  pure a = Pat (Tape . H.fromList . fmap ((`Entry` a) . snd) . spanSplit)
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

instance (IsString s) => IsString (Pat s) where
  fromString = pure . fromString

-- LAW TO VERIFY
-- forall p a. patRun p a == spanSplit a >>= \(_, a') -> fmap (_) (patRun p a')

-- Sketch: split arc into cycles, for each render the pattern over the cycle, slowing by length, then speed everything
-- up by whole amount to fit all into one cycle
goC :: Rational -> NESeq (Pat a, Rational) -> Arc -> Tape a
goC w pats arc = foldl' go1 mempty (spanSplit arc)
 where
  go1 t (i, Span subArc _) = t <> tapeFastBy i w (snd (go2 i subArc))
  go2 i subArc = foldl' (go3 i subArc) (0, mempty) pats
  go3 i subArc (o, t) (p, v) =
    (o + v, t <> tapeLateBy o (tapeSlowBy i v (unPat p subArc)))

patConcat :: NESeq (Pat a, Rational) -> Pat a
patConcat pats =
  let w = getSum (foldMap1' (Sum . snd) pats)
  in  Pat (goC w pats)

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

patRun :: Pat a -> Arc -> [Ev a]
patRun pa arc = tapeToList (unPat pa arc)

patTimeMapInv :: (Time -> Time) -> (Time -> Time) -> Pat a -> Pat a
patTimeMapInv onTape onArc (Pat k) = Pat (tapeTimeMapMono onTape . k . arcTimeMapMono onArc)

patAdjust :: (a -> Pat b -> Pat c) -> Pat a -> Pat b -> Pat c
patAdjust f pa pb = patInnerBind pa (`f` pb)

patFastBy, patSlowBy :: Rational -> Pat a -> Pat a
patFastBy t = patTimeMapInv (/ t) (* t)
patSlowBy t = patTimeMapInv (* t) (/ t)

patFast, patSlow :: Pat Rational -> Pat a -> Pat a
patFast = patAdjust patFastBy
patSlow = patAdjust patSlowBy

patEarlyBy, patLateBy :: Time -> Pat a -> Pat a
patEarlyBy t = patTimeMapInv id (subtract t)
patLateBy t = patTimeMapInv id (+ t)

patEarly, patLate :: Pat Time -> Pat a -> Pat a
patEarly = patAdjust patEarlyBy
patLate = patAdjust patLateBy

patDegradeBy :: Rational -> Pat a -> Pat a
patDegradeBy _ = id -- TODO fix it

patDegrade :: Pat Rational -> Pat a -> Pat a
patDegrade = patAdjust patDegradeBy

patCont :: (Time -> a) -> Pat a
patCont f = Pat (tapeSingleton . evCont f)

-- TODO move to module with continuous primitives
-- fnSine :: Rational -> Time -> Double
-- fnSine freq t = sin (2 * pi * fromRational (freq * t))
--
-- patSine :: Rational -> Pat Double
-- patSine = patCont . fnSine
