-- | Streams are functions from time to events. We can interpret patterns as streams
-- or construct them by hand. The stream primitives here come from Tidal, including
-- the different bind variants.
module Minipat.Stream
  ( Ev (..)
  , evCont
  , Tape
  , tapeFilter
  , tapeFastBy
  , tapeSlowBy
  , tapeEarlyBy
  , tapeLateBy
  , tapeDegradeBy
  , tapeUncons
  , tapeSingleton
  , tapeToList
  , tapeConcatMap
  , tapeFromList
  , Stream (..)
  , streamFilter
  , streamInnerBind
  , streamOuterBind
  , streamMixBind
  , streamRun
  , streamAdjust
  , streamConcat
  , streamReplicate
  , streamFastBy
  , streamSlowBy
  , streamFast
  , streamSlow
  , streamEarlyBy
  , streamLateBy
  , streamEarly
  , streamLate
  , streamDegradeBy
  , streamDegrade
  , streamCont
  )
where

import Control.Monad (ap)
import Data.Foldable (foldl', toList)
import Data.Foldable1 (foldMap1')
import Data.Heap (Entry (..), Heap)
import Data.Heap qualified as H
import Data.Semigroup (Semigroup (..), Sum (..))
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.String (IsString (..))
import Minipat.Rand (randFrac, spanSeed)
import Minipat.Time
  ( Arc (..)
  , Cycle (..)
  , CycleDelta (..)
  , CycleTime (..)
  , Span (..)
  , arcIntersect
  , arcTimeMapMono
  , spanSplit
  , spanTimeMapMono
  , spanWholeMapMono
  )

data Ev a = Ev
  { evSpan :: !Span
  , evValue :: !a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

evCont :: (CycleTime -> a) -> Arc -> Ev a
evCont f arc = Ev (Span arc Nothing) (f (arcStart arc))

newtype Tape a = Tape {unTape :: Heap (Entry Span a)}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Semigroup, Monoid)

instance Functor Tape where
  fmap f = Tape . H.mapMonotonic (\(Entry s a) -> Entry s (f a)) . unTape

tapeFilter :: (a -> Bool) -> Tape a -> Tape a
tapeFilter f = Tape . H.filter (\(Entry _ a) -> f a) . unTape

tapeFastBy :: Cycle -> Rational -> Tape a -> Tape a
tapeFastBy o r =
  let o' = fromInteger (unCycle o)
  in  tapeTimeMapMono (\(CycleTime t) -> CycleTime ((t - o') / r + o'))

tapeSlowBy :: Cycle -> Rational -> Tape a -> Tape a
tapeSlowBy o r =
  let o' = fromInteger (unCycle o)
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

newtype Stream a = Stream {unStream :: Arc -> Tape a}

instance Functor Stream where
  fmap f (Stream k) = Stream (fmap f . k)

instance Applicative Stream where
  pure a = Stream (Tape . H.fromList . fmap ((`Entry` a) . snd) . spanSplit)
  (<*>) = ap

instance Monad Stream where
  (>>=) = streamMixBind

instance Semigroup (Stream a) where
  Stream k1 <> Stream k2 = Stream (\arc -> k1 arc <> k2 arc)
  sconcat ss = Stream (\arc -> sconcat (fmap (`unStream` arc) ss))

instance Monoid (Stream a) where
  mempty = Stream (const mempty)
  mconcat ss = Stream (\arc -> mconcat (fmap (`unStream` arc) ss))

-- TODO Is there a useful instance here?
-- Maybe split into cycles and check emptiness L->R
-- instance Alternative Stream where
--   empty = mempty
--   (<|>) = (<>)

instance (IsString s) => IsString (Stream s) where
  fromString = pure . fromString

-- LAW TO VERIFY
-- forall p a. streamRun p a == spanSplit a >>= \(_, a') -> fmap (_) (streamRun p a')

streamFilter :: (a -> Bool) -> Stream a -> Stream a
streamFilter f (Stream k) = Stream (tapeFilter f . k)

streamBindWith :: (Maybe Arc -> Maybe Arc -> Maybe Arc) -> Stream a -> (a -> Stream b) -> Stream b
streamBindWith g pa f = Stream $ \arc ->
  let ta = unStream pa arc
  in  flip tapeConcatMap ta $ \(Ev (Span ac wh) a) ->
        let tb = unStream (f a) ac
        in  tapeWholeMapMono (g wh) tb

streamInnerBind :: Stream a -> (a -> Stream b) -> Stream b
streamInnerBind = streamBindWith (\_ x -> x)

streamOuterBind :: Stream a -> (a -> Stream b) -> Stream b
streamOuterBind = streamBindWith const

streamMixBind :: Stream a -> (a -> Stream b) -> Stream b
streamMixBind = streamBindWith (liftA2 arcIntersect)

streamRun :: Stream a -> Arc -> [Ev a]
streamRun pa arc = tapeToList (unStream pa arc)

streamTimeMapInv :: (CycleTime -> CycleTime) -> (CycleTime -> CycleTime) -> Stream a -> Stream a
streamTimeMapInv onTape onArc (Stream k) = Stream (tapeTimeMapMono onTape . k . arcTimeMapMono onArc)

streamAdjust :: (a -> Stream b -> Stream c) -> Stream a -> Stream b -> Stream c
streamAdjust f pa pb = streamInnerBind pa (`f` pb)

streamFastBy, streamSlowBy :: Rational -> Stream a -> Stream a
streamFastBy t = streamTimeMapInv (CycleTime . (/ t) . unCycleTime) (CycleTime . (* t) . unCycleTime)
streamSlowBy t = streamTimeMapInv (CycleTime . (* t) . unCycleTime) (CycleTime . (/ t) . unCycleTime)

streamFast, streamSlow :: Stream Rational -> Stream a -> Stream a
streamFast = streamAdjust streamFastBy
streamSlow = streamAdjust streamSlowBy

streamEarlyBy, streamLateBy :: CycleTime -> Stream a -> Stream a
streamEarlyBy t = streamTimeMapInv id (subtract t)
streamLateBy t = streamTimeMapInv id (+ t)

streamEarly, streamLate :: Stream CycleTime -> Stream a -> Stream a
streamEarly = streamAdjust streamEarlyBy
streamLate = streamAdjust streamLateBy

streamDegradeBy :: Rational -> Stream a -> Stream a
streamDegradeBy r (Stream k) = Stream (tapeDegradeBy r . k)

streamDegrade :: Stream Rational -> Stream a -> Stream a
streamDegrade = streamAdjust streamDegradeBy

-- Sketch: split arc into cycles, for each render the streamtern over the cycle, slowing by length, then speed everything
-- up by whole amount to fit all into one cycle
goConcat :: CycleDelta -> NESeq (Stream a, CycleDelta) -> Arc -> Tape a
goConcat w streams arc = foldl' go1 mempty (spanSplit arc)
 where
  go1 t (i, Span subArc _) = t <> tapeFastBy i (unCycleDelta w) (snd (go2 i subArc))
  go2 i subArc = foldl' (go3 i subArc) (0, mempty) streams
  go3 i subArc (o, t) (p, v) =
    (o + v, t <> tapeLateBy o (tapeSlowBy i (unCycleDelta v) (unStream p subArc)))

streamConcat :: NESeq (Stream a, CycleDelta) -> Stream a
streamConcat streams =
  let w = getSum (foldMap1' (Sum . snd) streams)
  in  Stream (goConcat w streams)

-- TODO implement stream repeat more efficiently than just using streamConcat
streamReplicate :: Int -> Stream a -> CycleDelta -> Stream a
streamReplicate n p v = streamConcat (NESeq.replicate n (p, v))

streamCont :: (CycleTime -> a) -> Stream a
streamCont f = Stream (tapeSingleton . evCont f)

-- TODO move to module with continuous primitives
-- fnSine :: Rational -> Time -> Double
-- fnSine freq t = sin (2 * pi * fromRational (freq * t))
--
-- streamSine :: Rational -> Stream Double
-- streamSine = streamCont . fnSine

-- streamDeriv :: Num n => Stream n -> Stream n
-- streamDeriv = undefined
--
-- streamInteg :: Num n => Stream n -> Stream n
-- streamInteg = undefined
