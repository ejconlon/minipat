{-# LANGUAGE OverloadedStrings #-}

-- | Interpreting patterns as streams
module Minipat.Interp
  ( InterpErr (..)
  , castInterpErr
  , interpPat
  , customInterpPat
  )
where

import Bowtie.Rewrite (AnnoErr (..), Rw, embedRw, peeksRw, throwRw)
import Control.Exception (Exception)
import Data.Ratio ((%))
import Data.Typeable (Typeable)
import Data.Void (Void)
import Minipat.Ast
  ( Degrade (..)
  , Elongate (..)
  , Euclid (..)
  , Group (..)
  , GroupType (..)
  , Mod (..)
  , ModType (..)
  , Pat (..)
  , PatF (..)
  , Poly (..)
  , Replicate (..)
  , Speed (..)
  , SpeedDir (..)
  , factorValue
  )
import Minipat.Pattern (PatM, Pattern (..), PatternUnwrap (..))
import Minipat.Rewrite (patRw)

-- | An error interpreting a 'Pat' as a 'Stream'
data InterpErr e
  = -- | When shorthands have not been previously eliminated by normalization
    InterpErrShort
  | -- | Wraps custom errors
    InterpErrCustom !e
  deriving stock (Eq, Ord, Show)

instance (Show e, Typeable e) => Exception (InterpErr e)

castInterpErr :: InterpErr Void -> InterpErr e
castInterpErr = \case InterpErrShort -> InterpErrShort

construct :: (PatternUnwrap b f) => PatM f (f x) -> Rw b e (f x)
construct = peeksRw . patUnwrap'

construct1 :: (PatternUnwrap b f) => PatM f (f x) -> Rw b e (f x, Rational)
construct1 = fmap (,1) . construct

guardFastBy :: (Pattern f) => Rational -> f x -> f x
guardFastBy r p = if r == 1 then p else patFastBy r p

goInterp
  :: (PatternUnwrap b f)
  => (a -> Either e (PatM f (f c)))
  -> PatF b a (f c, Rational)
  -> Rw b (InterpErr e) (f c, Rational)
goInterp useValue = \case
  PatPure a -> either (throwRw . InterpErrCustom) construct1 (useValue a)
  PatSilence -> construct1 patEmpty'
  PatShort _ -> throwRw InterpErrShort
  PatGroup (Group _ ty els) -> do
    case ty of
      GroupTypeSeq _ -> construct1 (patSeq' els)
      GroupTypePar -> construct1 (patPar' (fmap fst els))
      GroupTypeRand ->
        let els'' = fmap (\(el, w) -> guardFastBy w el) els
        in  construct1 (patRand' els'')
      GroupTypeAlt ->
        let els'' = fmap (\(el, w) -> guardFastBy w el) els
        in  construct1 (patAlt' els'')
  PatMod (Mod (el, w) md) -> do
    case md of
      ModTypeSpeed (Speed dir spat) -> do
        spat' <- embedRw (recInterpPat spat)
        let f = case dir of
              SpeedDirFast -> patFast
              SpeedDirSlow -> patSlow
            spat'' = fmap factorValue spat'
            el' = f spat'' el
        pure (el', w)
      ModTypeDegrade (Degrade mdpat) -> do
        dpat' <- case mdpat of
          Nothing -> pure (patPure (1 % 2))
          Just dpat -> fmap (fmap factorValue) (embedRw (recInterpPat dpat))
        let el' = patDeg dpat' el
        pure (el', w)
      ModTypeEuclid euc -> do
        el' <- construct (patEuc' euc el)
        let w' = fromInteger (eucSteps euc)
        pure (el', w')
      ModTypeElongate (Elongate f) -> do
        let w' = factorValue f * w
        pure (el, w')
      ModTypeReplicate (Replicate mi) -> do
        let v = maybe 2 fromInteger mi
        el' <- construct (patRep' v el)
        let w' = fromIntegral v
        pure (el', w')
  PatPoly (Poly _ _) -> error "TODO"

customInterpPat
  :: (PatternUnwrap b f) => (a -> Either e (PatM f (f c))) -> Pat b a -> Either (AnnoErr b (InterpErr e)) (f c)
customInterpPat use = fmap fst . patRw (goInterp use)

recInterpPat :: (PatternUnwrap b f) => Pat b a -> Either (AnnoErr b (InterpErr e)) (f a)
recInterpPat = either (\(AnnoErr b e) -> Left (AnnoErr b (castInterpErr e))) Right . interpPat

interpPat :: (PatternUnwrap b f) => Pat b a -> Either (AnnoErr b (InterpErr Void)) (f a)
interpPat = customInterpPat (Right . patPure')
