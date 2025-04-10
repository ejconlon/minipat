-- | Streams are functions from time to events. We can interpret patterns as streams
-- or construct them by hand. The stream primitives here come from Tidal, including
-- the different bind variants.
module Minipat.Stream
  ( Stream
  , streamFilter
  , streamBind
  , streamApply
  , streamRun
  , streamAdjust
  , streamSeq
  , streamRel
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
  , streamEuc
  , streamRand
  , streamAlt
  , streamPar
  , streamSwitch
  , streamPieces
  , streamNudge
  , streamChop
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (ap)
import Control.Monad.Identity (Identity (..))
import Data.Foldable (foldMap', foldl')
import Data.Heap (Entry (..))
import Data.Heap qualified as H
import Data.Semigroup (Semigroup (..))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Minipat.Ast (Euclid (..))
import Minipat.Classes (Flow (..), Pattern (..), PatternUnwrap (..))
import Minipat.Rand (arcSeed, randInt)
import Minipat.Tape (Ev (..), Tape (..))
import Minipat.Tape qualified as T
import Minipat.Time
  ( Arc (..)
  , CycleArc
  , CycleDelta (..)
  , CycleTime (..)
  , MergeStrat (..)
  , Span (..)
  , arcMerge
  , spanSplit
  )

newtype Stream a = Stream {unStream :: CycleArc -> Tape CycleTime a}

instance Functor Stream where
  fmap f s = Stream (fmap f . unStream s)

instance Applicative Stream where
  pure a = Stream (Tape . H.fromList . fmap ((`Entry` a) . snd) . spanSplit)
  (<*>) = ap

instance Monad Stream where
  (>>=) = streamBind MergeStratMixed

-- | '(<>)' is parallel composition of streams
instance Semigroup (Stream a) where
  Stream k1 <> Stream k2 = Stream (\arc -> k1 arc <> k2 arc)
  sconcat ss = Stream (\arc -> sconcat (fmap (`unStream` arc) ss))

-- | 'mempty' is the empty stream
instance Monoid (Stream a) where
  mempty = Stream (const mempty)
  mconcat ss = Stream (\arc -> mconcat (fmap (`unStream` arc) ss))

instance Alternative Stream where
  empty = mempty
  (<|>) = (<>)

streamFilter :: (a -> Bool) -> Stream a -> Stream a
streamFilter f s = Stream (T.tapeFilter f . unStream s)

streamBindWith :: (Maybe CycleArc -> Maybe CycleArc -> Maybe CycleArc) -> Stream a -> (a -> Stream b) -> Stream b
streamBindWith g pa f = Stream $ \arc ->
  let ta = unStream pa arc
  in  flip T.tapeConcatMap ta $ \(Ev (Span ac wh) a) ->
        let tb = unStream (f a) ac
        in  T.tapeWholeMap (g wh) tb

streamBind :: MergeStrat -> Stream a -> (a -> Stream b) -> Stream b
streamBind = streamBindWith . arcMerge

streamApplyWith
  :: (Maybe CycleArc -> Maybe CycleArc -> Maybe CycleArc) -> (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamApplyWith g f pa = streamBindWith g (fmap f pa) . flip fmap

streamApply :: MergeStrat -> (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamApply = streamApplyWith . arcMerge

streamRun :: Stream a -> CycleArc -> Tape CycleTime a
-- TODO necessary?
-- streamRun pa arc =
--   let arc' = arcWiden arc
--   in  if arc' == arc
--         then unStream pa arc
--         else T.tapeRelevant arc (unStream pa arc')
streamRun = unStream

streamTimeMapInv :: (CycleTime -> CycleTime) -> (CycleTime -> CycleTime) -> Stream a -> Stream a
streamTimeMapInv onTape onArc s = Stream (T.tapeTimeMapMono onTape . unStream s . fmap onArc)

streamHold :: (Eq a) => a -> Stream a -> Stream a
streamHold z0 s = Stream (\ac0 -> T.tapeHold ac0 z0 (unStream s ac0))

streamAdjust :: (Eq a) => a -> (a -> Stream b -> Stream c) -> Stream a -> Stream b -> Stream c
streamAdjust z0 f pa pb = streamBind MergeStratInner (streamHold z0 pa) (`f` pb)

streamFastBy, streamSlowBy :: Rational -> Stream a -> Stream a
streamFastBy t = streamTimeMapInv (CycleTime . (/ t) . unCycleTime) (CycleTime . (* t) . unCycleTime)
streamSlowBy t = streamTimeMapInv (CycleTime . (* t) . unCycleTime) (CycleTime . (/ t) . unCycleTime)

streamFast, streamSlow :: Stream Rational -> Stream a -> Stream a
streamFast = streamAdjust 1 streamFastBy
streamSlow = streamAdjust 1 streamSlowBy

streamEarlyBy, streamLateBy :: CycleDelta -> Stream a -> Stream a
streamEarlyBy (CycleDelta t) = streamTimeMapInv id (CycleTime . subtract t . unCycleTime)
streamLateBy (CycleDelta t) = streamTimeMapInv id (CycleTime . (+ t) . unCycleTime)

streamEarly, streamLate :: Stream CycleDelta -> Stream a -> Stream a
streamEarly = streamAdjust 0 streamEarlyBy
streamLate = streamAdjust 0 streamLateBy

streamDegBy :: Rational -> Stream a -> Stream a
streamDegBy r s = Stream (T.tapeDegradeBy r . unStream s)

streamDeg :: Stream Rational -> Stream a -> Stream a
streamDeg = streamAdjust 1 streamDegBy

streamSeq :: Seq (Stream a) -> Stream a
streamSeq = streamRel . fmap (,1)

streamRel :: Seq (Stream a, Rational) -> Stream a
streamRel ss = Stream $ \arc ->
  -- Sketch: split arc into cycles, for each render each stream over the cycle, slowing
  -- by length, then speed everything up by whole amount to fit all into one cycle
  let w = sum (fmap snd ss)
      go1 (i, Span subArc _) = T.tapeFastBy (fromInteger i) w (snd (go2 i subArc))
      go2 i subArc = foldl' (go3 i subArc) (0, mempty) ss
      go3 i subArc (!o, !t) (p, v) =
        (o + v, t <> T.tapeLateBy (CycleDelta o) (T.tapeSlowBy (fromInteger i) v (unStream p subArc)))
  in  mconcat (fmap go1 (spanSplit arc))

streamRep :: Integer -> Stream a -> Stream a
streamRep n s = Stream $ \arc ->
  -- Sketch: split arc into cycles, for each render the stream over the cycle,
  -- shift and concatenate n times, then speed everything up to fit into one cycle
  let go1 (i, Span subArc _) = T.tapeFastBy (fromInteger i) (fromInteger n) (go2 subArc)
      go2 subArc =
        let t = unStream s subArc
        in  mconcat (fmap (\k -> T.tapeLateBy (fromIntegral k) t) [0 .. n - 1])
  in  mconcat (fmap go1 (spanSplit arc))

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
  in  streamRel eucSeq

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
        let i = mod (fromInteger z) l
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

streamNudge :: (CycleArc -> CycleArc) -> Stream a -> Stream a
streamNudge f sa = Stream (\arc -> T.tapeNudge f arc (unStream sa arc))

streamChop :: (Ev CycleTime a -> Tape CycleTime b) -> Stream a -> Stream b
streamChop f sa = Stream (T.tapeConcatMap f . unStream sa)

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
  patRel' = Identity . streamRel
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

instance Flow Stream where
  flowApply = streamApply
  flowFilter = streamFilter
  flowEarlyBy = streamEarlyBy
  flowLateBy = streamLateBy
  flowEarly = streamEarly
  flowLate = streamLate
  flowSwitch = streamSwitch
  flowPieces = streamPieces
  flowNudge = streamNudge
  flowChop = streamChop
