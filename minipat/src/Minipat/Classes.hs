module Minipat.Classes
  ( Pattern (..)
  , PatternUnwrap (..)
  , Flow (..)
  )
where

import Bowtie (pattern JotP)
import Control.Applicative (Alternative (..))
import Control.Monad.Identity (Identity (..))
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
import Minipat.EStream
import Minipat.Stream
import Minipat.Time (CycleDelta, CycleTime)

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

mkPatSeq :: Seq (Pat b a, Rational) -> Reader b (Pat b a)
mkPatSeq = \case
  Empty -> mkPat PatSilence
  (x, _) :<| Empty -> pure x
  xs ->
    let adjust = unPat . fst
    in  mkPat (PatGroup (Group 1 (GroupTypeSeq SeqPresSpace) (fmap adjust xs)))

-- | 'Pat' and 'Stream' can be constructed abstractly with this
class (Functor f, Monad (PatM f), Default (PatA f)) => Pattern f where
  type PatM f :: Type -> Type
  type PatA f :: Type

  patCon' :: PatM f (f a) -> PatA f -> f a
  patCon :: PatM f (f a) -> f a
  patCon = flip patCon' def

  patPure' :: a -> PatM f (f a)
  patPure :: a -> f a
  patPure = patCon . patPure'

  patEmpty' :: PatM f (f a)
  patEmpty :: f a
  patEmpty = patCon patEmpty'

  patPar' :: Seq (f a) -> PatM f (f a)
  patPar :: Seq (f a) -> f a
  patPar = patCon . patPar'

  patAlt' :: Seq (f a) -> PatM f (f a)
  patAlt :: Seq (f a) -> f a
  patAlt = patCon . patAlt'

  patRand' :: Seq (f a) -> PatM f (f a)
  patRand :: Seq (f a) -> f a
  patRand = patCon . patRand'

  patSeq' :: Seq (f a, Rational) -> PatM f (f a)
  patSeq :: Seq (f a, Rational) -> f a
  patSeq = patCon . patSeq'

  patEuc' :: Euclid -> f a -> PatM f (f a)
  patEuc :: Euclid -> f a -> f a
  patEuc e = patCon . patEuc' e

  patRep' :: Integer -> f a -> PatM f (f a)
  patRep :: Integer -> f a -> f a
  patRep i = patCon . patRep' i

  patFast', patSlow' :: f Rational -> f a -> PatM f (f a)
  patFast :: f Rational -> f a -> f a
  patFast p = patCon . patFast' p
  patSlow :: f Rational -> f a -> f a
  patSlow p = patCon . patSlow' p

  patFastBy', patSlowBy' :: Rational -> f a -> PatM f (f a)
  patFastBy, patSlowBy :: Rational -> f a -> f a
  patFastBy r = patCon . patFastBy' r
  patSlowBy r = patCon . patSlowBy' r

  patDeg' :: f Rational -> f a -> PatM f (f a)
  patDeg :: f Rational -> f a -> f a
  patDeg p = patCon . patDeg' p

  patDegBy' :: Rational -> f a -> PatM f (f a)
  patDegBy :: Rational -> f a -> f a
  patDegBy r = patCon . patDegBy' r

-- | Sometimes you can construct patterns with other types of annotations.
class (Pattern f) => PatternUnwrap b f where
  patUnwrap' :: PatM f (f a) -> b -> f a

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
  patEuc' = mkPatMod . ModTypeEuclid
  patRep' = mkPatRep
  patFast' = mkPatSpeed SpeedDirFast
  patSlow' = mkPatSpeed SpeedDirSlow
  patFastBy' = mkPatSpeedBy SpeedDirFast
  patSlowBy' = mkPatSpeedBy SpeedDirSlow
  patDeg' = mkPatDeg
  patDegBy' = mkPatDegBy

instance (Default b) => PatternUnwrap b (Pat b) where
  patUnwrap' = patCon'

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

instance Pattern EStream where
  type PatM EStream = Identity
  type PatA EStream = ()
  patCon' = const . runIdentity
  patPure' = Identity . pure
  patEmpty' = Identity mempty
  patPar' = Identity . estreamPar
  patAlt' = Identity . estreamAlt
  patRand' = Identity . estreamRand
  patSeq' = Identity . estreamSeq
  patEuc' e = Identity . estreamEuc e
  patRep' r = Identity . estreamRep r
  patFast' p = Identity . estreamFast p
  patSlow' p = Identity . estreamSlow p
  patFastBy' r = Identity . estreamFastBy r
  patSlowBy' r = Identity . estreamSlowBy r
  patDeg' p = Identity . estreamDeg p
  patDegBy' r = Identity . estreamDegBy r

instance PatternUnwrap b EStream where
  patUnwrap' = const . runIdentity

class (Alternative f, Pattern f) => Flow f where
  flowFilter :: (a -> Bool) -> f a -> f a
  flowEarlyBy, flowLateBy :: CycleDelta -> f a -> f a
  flowEarly, flowLate :: f CycleDelta -> f a -> f a
  flowSwitch :: f a -> CycleTime -> f a -> f a
  flowPieces :: f a -> Seq (CycleTime, f a) -> f a

instance Flow Stream where
  flowFilter = streamFilter
  flowEarlyBy = streamEarlyBy
  flowLateBy = streamLateBy
  flowEarly = streamEarly
  flowLate = streamLate
  flowSwitch = streamSwitch
  flowPieces = streamPieces

instance Flow EStream where
  flowFilter = estreamFilter
  flowEarlyBy = estreamEarlyBy
  flowLateBy = estreamLateBy
  flowEarly = estreamEarly
  flowLate = estreamLate
  flowSwitch = estreamSwitch
  flowPieces = estreamPieces
