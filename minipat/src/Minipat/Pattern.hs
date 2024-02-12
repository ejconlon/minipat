module Minipat.Pattern
  ( Pattern (..)
  , PatternUnwrap (..)
  )
where

import Bowtie (pattern JotP)
import Control.Monad.Reader (Reader, asks, runReader)
import Data.Default (Default (..))
import Data.Foldable (toList)
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

mkPat :: PatF b a (UnPat b a) -> Reader b (Pat b a)
mkPat pf = asks (\b -> Pat (JotP b pf))

mkPatGroup :: GroupType -> Seq (Pat b a) -> Reader b (Pat b a)
mkPatGroup gt = \case
  Empty -> mkPat PatSilence
  x :<| Empty -> pure x
  xs -> mkPat (PatGroup (Group 0 gt (fmap unPat xs)))

mkPatMod :: ModType b -> Pat b a -> Reader b (Pat b a)
mkPatMod mt (Pat pa) = mkPat (PatMod (Mod pa mt))

mkPatSpeed :: SpeedDir -> Pat b Rational -> Pat b a -> Reader b (Pat b a)
mkPatSpeed sd pf = mkPatMod (ModTypeSpeed (Speed sd (fmap factorFromRational pf)))

mkPatDeg :: Pat b Rational -> Pat b a -> Reader b (Pat b a)
mkPatDeg pf = mkPatMod (ModTypeDegrade (Degrade (Just (fmap factorFromRational pf))))

mkPatRep :: Integer -> Pat b a -> Reader b (Pat b a)
mkPatRep n = mkPatMod (ModTypeReplicate (Replicate (Just n)))

mkPatSeq :: (Default b) => Seq (Pat b a, Rational) -> Reader b (Pat b a)
mkPatSeq = \case
  Empty -> mkPat PatSilence
  (x, _) :<| Empty -> pure x
  xs ->
    let w = sum (fmap snd (toList xs))
        adjust (x, t) = unPat (patFastBy (t / w) x)
    in  mkPat (PatGroup (Group 0 (GroupTypeSeq SeqPresSpace) (fmap adjust xs)))

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
  patSlowBy r = patCon . patFastBy' r

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
  patFastBy' = patFast' . patPure -- patFast . patPure
  patSlowBy' = patSlow' . patPure -- patSlow . patPure
  patDeg' = mkPatDeg
  patDegBy' = patDeg' . patPure

instance (Default b) => PatternUnwrap b (Pat b) where
  patUnwrap' = patCon'
