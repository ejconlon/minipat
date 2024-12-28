-- | Classes that generalize patterns and streams.
module Minipat.Classes
  ( Pattern (..)
  , PatternUnwrap (..)
  , Flow (..)
  )
where

import Bowtie (pattern JotP)
import Control.Applicative (Alternative (..))
import Control.Monad.Reader (Reader, asks, runReader)
import Data.Default (Default (..))
import Data.Kind (Type)
import Data.Sequence (Seq (..))
import Minipat.Ast
  ( Degrade (..)
  , Euclid
  , Group (..)
  , GroupType (..)
  , Mod (..)
  , ModType (..)
  , Pat (..)
  , PatF (..)
  , Replicate (..)
  , SeqPres (..)
  , Speed (..)
  , SpeedDir (..)
  , UnPat
  , factorFromRational
  )
import Minipat.Tape (Ev, Tape)
import Minipat.Time (CycleArc, CycleDelta, CycleTime, MergeStrat (..))

mkPat :: PatF b a (UnPat b a) -> Reader b (Pat b a)
mkPat pf = asks (\b -> Pat (JotP b pf))

mkPatGroup :: GroupType -> Seq (Pat b a) -> Reader b (Pat b a)
mkPatGroup gt = \case
  Empty -> mkPat PatSilence
  x :<| Empty -> pure x
  xs -> mkPat (PatGroup (Group 1 gt (fmap unPat xs)))

mkPatMod :: ModType b -> Pat b a -> Reader b (Pat b a)
mkPatMod mt (Pat pa) = mkPat (PatMod (Mod pa mt))

mkPatSpeedBy :: (Default b) => SpeedDir -> Rational -> Pat b a -> Reader b (Pat b a)
mkPatSpeedBy sd f p =
  if f == 1
    then pure p
    else mkPatSpeed sd (patPure f) p

mkPatSpeed :: SpeedDir -> Pat b Rational -> Pat b a -> Reader b (Pat b a)
mkPatSpeed sd pf = mkPatMod (ModTypeSpeed (Speed sd (fmap factorFromRational pf)))

mkPatDegBy :: (Default b) => Rational -> Pat b a -> Reader b (Pat b a)
mkPatDegBy f p =
  if
    | f <= 0 -> patEmpty'
    | f >= 1 -> pure p
    | otherwise -> mkPatDeg (patPure f) p

mkPatDeg :: Pat b Rational -> Pat b a -> Reader b (Pat b a)
mkPatDeg pf = mkPatMod (ModTypeDegrade (Degrade (Just (fmap factorFromRational pf))))

mkPatRep :: Integer -> Pat b a -> Reader b (Pat b a)
mkPatRep n = mkPatMod (ModTypeReplicate (Replicate (Just n)))

mkPatSeq :: Seq (Pat b a) -> Reader b (Pat b a)
mkPatSeq = mkPatRel . fmap (,1)

mkPatRel :: Seq (Pat b a, Rational) -> Reader b (Pat b a)
mkPatRel = \case
  Empty -> mkPat PatSilence
  (x, _) :<| Empty -> pure x
  xs ->
    let adjust = unPat . fst
    in  mkPat (PatGroup (Group 1 (GroupTypeSeq SeqPresSpace) (fmap adjust xs)))

-- | 'Pat' and 'Stream' can be constructed abstractly with this. The 'PatM' and
-- 'PatA' associated types are to generalize operations between 'Pat' (which is
-- annotated at every level with source locations) and 'Stream' (which has no such
-- annotations). "Unprimed" variants eliminate monad layers with default annotations.
class (Functor f, Monad (PatM f), Default (PatA f)) => Pattern f where
  -- | The monad `f` is constructed in.
  type PatM f :: Type -> Type

  -- | The annotation `f` requires for construction.
  type PatA f :: Type

  -- | "Unwraps" 'PatM'
  patCon' :: PatM f (f a) -> PatA f -> f a

  patCon :: PatM f (f a) -> f a
  patCon = flip patCon' def

  -- | 'Applicative' pure - single event repeats every cycle
  patPure' :: a -> PatM f (f a)

  patPure :: a -> f a
  patPure = patCon . patPure'

  -- | Empty pattern (no events)
  patEmpty' :: PatM f (f a)

  patEmpty :: f a
  patEmpty = patCon patEmpty'

  -- | Parallel composition (all in one cycle, looping)
  patPar' :: Seq (f a) -> PatM f (f a)

  patPar :: Seq (f a) -> f a
  patPar = patCon . patPar'

  -- | Alternating composition (one per cycle, looping)
  patAlt' :: Seq (f a) -> PatM f (f a)

  patAlt :: Seq (f a) -> f a
  patAlt = patCon . patAlt'

  -- | Random composition (one per cycle, drawing with replacement)
  patRand' :: Seq (f a) -> PatM f (f a)

  patRand :: Seq (f a) -> f a
  patRand = patCon . patRand'

  -- | Sequential composition (all compressed to one cycle, looping)
  patSeq' :: Seq (f a) -> PatM f (f a)

  patSeq :: Seq (f a) -> f a
  patSeq = patCon . patSeq'

  -- | Relative sequential composition (given fractional widths)
  patRel' :: Seq (f a, Rational) -> PatM f (f a)

  patRel :: Seq (f a, Rational) -> f a
  patRel = patCon . patRel'

  -- | Euclidean repetition
  patEuc' :: Euclid -> f a -> PatM f (f a)

  patEuc :: Euclid -> f a -> f a
  patEuc e = patCon . patEuc' e

  -- | Simple repetition
  patRep' :: Integer -> f a -> PatM f (f a)

  patRep :: Integer -> f a -> f a
  patRep i = patCon . patRep' i

  -- | Adjusts the speed of playback, squeezing more or less into a cycle
  -- These variants accept the speed factor as a time-varying signal.
  patFast', patSlow' :: f Rational -> f a -> PatM f (f a)

  patFast :: f Rational -> f a -> f a
  patFast p = patCon . patFast' p
  patSlow :: f Rational -> f a -> f a
  patSlow p = patCon . patSlow' p

  -- | Adjusts the speed of playback, squeezing more or less into a cycle
  -- These variants accept a constant speed factor.
  patFastBy', patSlowBy' :: Rational -> f a -> PatM f (f a)

  patFastBy, patSlowBy :: Rational -> f a -> f a
  patFastBy r = patCon . patFastBy' r
  patSlowBy r = patCon . patSlowBy' r

  -- | "Degrades" playback by omitting events with the given probability.
  -- This variant accepts the probability as a time-varying signal.
  patDeg' :: f Rational -> f a -> PatM f (f a)

  patDeg :: f Rational -> f a -> f a
  patDeg p = patCon . patDeg' p

  -- | "Degrades" playback by omitting events with the given probability.
  -- This variant accepts a constant probability.
  patDegBy' :: Rational -> f a -> PatM f (f a)

  patDegBy :: Rational -> f a -> f a
  patDegBy r = patCon . patDegBy' r

instance (Default b) => Pattern (Pat b) where
  type PatM (Pat b) = Reader b
  type PatA (Pat b) = b
  patCon' = runReader
  patPure' = mkPat . PatPure
  patEmpty' = mkPat PatSilence
  patPar' = mkPatGroup GroupTypePar
  patAlt' = mkPatGroup GroupTypeAlt
  patRand' = mkPatGroup GroupTypeRand
  patSeq' = mkPatSeq
  patRel' = mkPatRel
  patEuc' = mkPatMod . ModTypeEuclid
  patRep' = mkPatRep
  patFast' = mkPatSpeed SpeedDirFast
  patSlow' = mkPatSpeed SpeedDirSlow
  patFastBy' = mkPatSpeedBy SpeedDirFast
  patSlowBy' = mkPatSpeedBy SpeedDirSlow
  patDeg' = mkPatDeg
  patDegBy' = mkPatDegBy

-- | Sometimes you can construct patterns with other types of annotations.
-- For example, you can eliminate the 'PatM' layer of a 'Stream' ('Identity')
-- with any old value.
class (Pattern f) => PatternUnwrap b f where
  patUnwrap' :: PatM f (f a) -> b -> f a

instance (Default b) => PatternUnwrap b (Pat b) where
  patUnwrap' = patCon'

-- | A 'Flow' exposes 'Applicative' and time-aware operations.
class (Alternative f, Pattern f) => Flow f where
  -- | 'Applicative' 'liftA2' with configurable merging of whole arcs.
  flowApply :: MergeStrat -> (a -> b -> c) -> f a -> f b -> f c

  flowInnerApply :: (a -> b -> c) -> f a -> f b -> f c
  flowInnerApply = flowApply MergeStratInner
  flowOuterApply :: (a -> b -> c) -> f a -> f b -> f c
  flowOuterApply = flowApply MergeStratOuter
  flowMixedApply :: (a -> b -> c) -> f a -> f b -> f c
  flowMixedApply = flowApply MergeStratMixed

  -- | Keeps events that satisfy the predicate.
  flowFilter :: (a -> Bool) -> f a -> f a

  -- | Shifts events in cycle time.
  -- These variants accept the delta as a time-varying signal.
  flowEarlyBy, flowLateBy :: CycleDelta -> f a -> f a

  -- | Shifts events in cycle time.
  -- These variants accept a constant delta.
  flowEarly, flowLate :: f CycleDelta -> f a -> f a

  -- | Switch from one flow to another at the given cycle time.
  flowSwitch :: f a -> CycleTime -> f a -> f a

  -- | Switch from one flow to the next sequentially.
  flowPieces :: f a -> Seq (CycleTime, f a) -> f a

  -- | Adjust cycle times with the given function.
  -- (See 'spanNudge' for constraints.)
  flowNudge :: (CycleArc -> CycleArc) -> f a -> f a

  -- | A kind of monadic bind for flows that applies the function
  -- at the tape level.
  flowChop :: (Ev a -> Tape b) -> f a -> f b
