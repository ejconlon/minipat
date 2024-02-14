-- | Streams are functions from time to events. We can interpret patterns as streams
-- or construct them by hand. The stream primitives here come from Tidal, including
-- the different bind variants.
module Minipat.Stream
  ( Ev (..)
  , evCont
  , Tape
  , tapeNull
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
  , streamSeq
  , streamRep
  , streamFastBy
  , streamSlowBy
  , streamFast
  , streamSlow
  , streamEarlyBy
  , streamLateBy
  , streamEarly
  , streamLate
  , streamDegBy
  , streamDeg
  , streamCont
  , streamEuc
  , streamRand
  , streamAlt
  , streamPar
  )
where

import Control.Monad (ap)
import Control.Monad.Identity (Identity (..))
import Data.Foldable (foldMap', foldl', toList)
import Data.Heap (Entry (..), Heap)
import Data.Heap qualified as H
import Data.Semigroup (Semigroup (..))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.String (IsString (..))
import Minipat.Ast (Euclid (..))
import Minipat.Pattern (Pattern (..), PatternUnwrap (..))
import Minipat.Rand (arcSeed, randFrac, randInt, spanSeed)
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
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as P

data Ev a = Ev
  { evSpan :: !Span
  , evValue :: !a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty a) => Pretty (Ev a) where
  pretty (Ev sp v) = P.hsep [pretty sp, pretty v]

evCont :: (CycleTime -> a) -> Arc -> Ev a
evCont f arc = Ev (Span arc Nothing) (f (arcStart arc))

newtype Tape a = Tape {unTape :: Heap (Entry Span a)}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Semigroup, Monoid)

instance Functor Tape where
  fmap f = Tape . H.mapMonotonic (\(Entry s a) -> Entry s (f a)) . unTape

tapeNull :: Tape a -> Bool
tapeNull = H.null . unTape

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

-- | '(<>)' is parallel composition of streams
instance Semigroup (Stream a) where
  Stream k1 <> Stream k2 = Stream (\arc -> k1 arc <> k2 arc)
  sconcat ss = Stream (\arc -> sconcat (fmap (`unStream` arc) ss))

-- | 'mempty' is the empty stream
instance Monoid (Stream a) where
  mempty = Stream (const mempty)
  mconcat ss = Stream (\arc -> mconcat (fmap (`unStream` arc) ss))

-- TODO alt should be invariant under shifts, but that is expensive
-- -- | 'empty', like 'mempty', is the empty stream
-- instance Alternative Stream where
--   empty = mempty
--   Stream k1 <|> Stream k2 = Stream (foldl' merge mempty . spanSplit) where
--     merge t0 (_, Span arc _) =
--       let t1 = k1 arc
--           t2 = if tapeNull t1 then k2 arc else t1
--       in t0 <> t2

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

streamDegBy :: Rational -> Stream a -> Stream a
streamDegBy r (Stream k) = Stream (tapeDegradeBy r . k)

streamDeg :: Stream Rational -> Stream a -> Stream a
streamDeg = streamAdjust streamDegBy

streamSeq :: Seq (Stream a, Rational) -> Stream a
streamSeq ss = Stream $ \arc ->
  -- Sketch: split arc into cycles, for each render each stream over the cycle, slowing
  -- by length, then speed everything up by whole amount to fit all into one cycle
  let w = sum (fmap snd ss)
      go1 (i, Span subArc _) = tapeFastBy i w (snd (go2 i subArc))
      go2 i subArc = foldl' (go3 i subArc) (0, mempty) ss
      go3 i subArc (!o, !t) (p, v) =
        (o + v, t <> tapeLateBy (CycleDelta o) (tapeSlowBy i v (unStream p subArc)))
  in  mconcat (fmap go1 (spanSplit arc))

streamRep :: Integer -> Stream a -> Stream a
streamRep n s = Stream $ \arc ->
  -- Sketch: split arc into cycles, for each render the stream over the cycle,
  -- shift and concatenate n times, then speed everything up to fit into one cycle
  let go1 (i, Span subArc _) = tapeFastBy i (fromInteger n) (go2 subArc)
      go2 subArc =
        let t = unStream s subArc
        in  mconcat (fmap (\k -> tapeLateBy (fromIntegral k) t) [0 .. n - 1])
  in  mconcat (fmap go1 (spanSplit arc))

streamCont :: (CycleTime -> a) -> Stream a
streamCont f = Stream (tapeSingleton . evCont f)

-- TODO implement this more efficiently than just concatenation?
streamEuc :: Euclid -> Stream a -> Stream a
streamEuc (Euclid (fromInteger -> filled) (fromInteger -> steps) (maybe 0 fromInteger -> shift)) s =
  let activeEl = (s, 1)
      passiveEl = (mempty, 1)
      eucSeq =
        Seq.fromFunction steps $ \ix0 ->
          let ix1 = ix0 + shift
              ix = if ix1 >= steps then ix1 - steps else ix1
              active = mod ix filled == 0
          in  if active then activeEl else passiveEl
  in  streamSeq eucSeq

streamRand :: Seq (Stream a) -> Stream a
streamRand ss =
  let l = Seq.length ss
      f arc =
        let s = arcSeed arc
            i = randInt l s
            t = Seq.index ss i
        in  unStream t arc
  in  Stream (foldMap' (f . spanActive . snd) . spanSplit)

streamAlt :: Seq (Stream a) -> Stream a
streamAlt ss =
  let l = Seq.length ss
      f z arc =
        let i = mod (fromInteger (unCycle z)) l
            t = Seq.index ss i
        in  unStream t arc
  in  Stream (foldMap' (\(z, sp) -> f z (spanActive sp)) . spanSplit)

streamPar :: Seq (Stream a) -> Stream a
streamPar = foldl' (<>) mempty

streamSwitch :: Stream a -> CycleTime -> Stream a -> Stream a
streamSwitch sa t sb = Stream $ \arc@(Arc s e) ->
  if
    | t <= s -> unStream sb arc
    | t >= e -> unStream sa arc
    | otherwise -> unStream sa (Arc s t) <> unStream sb (Arc t e)

streamPieces :: Stream a -> Seq (CycleTime, Stream a) -> Stream a
streamPieces x = \case
  Empty -> x
  (t, x') :<| xs' -> streamSwitch x t (streamPieces x' xs')

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

instance Pattern Stream where
  type PatM Stream = Identity
  type PatA Stream = ()
  patCon' = const . runIdentity
  patPure' = Identity . pure
  patEmpty' = Identity mempty
  patPar' = Identity . streamPar
  patAlt' = Identity . streamAlt
  patRand' = Identity . streamRand
  patSeq' = Identity . streamSeq
  patEuc' e = Identity . streamEuc e
  patRep' r = Identity . streamRep r
  patFast' p = Identity . streamFast p
  patSlow' p = Identity . streamSlow p
  patFastBy' r = Identity . streamFastBy r
  patSlowBy' r = Identity . streamSlowBy r
  patDeg' p = Identity . streamDeg p
  patDegBy' r = Identity . streamDegBy r

instance PatternUnwrap b Stream where
  patUnwrap' = const . runIdentity
