{-# LANGUAGE OverloadedStrings #-}

-- | Interpreting patterns as streams
module Minipat.Interp
  ( InterpErr (..)
  , castInterpErr
  , interpPat
  , customInterpPat
  )
where

import Bowtie.Rewrite (AnnoErr (..), Rw, embedRw, throwRw)
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
  , Pattern (..)
  , Poly (..)
  , Replicate (..)
  , Speed (..)
  , SpeedDir (..)
  , factorValue
  )
import Minipat.Rewrite (patRw)

-- | An error interpreting a 'Pat' as a 'Stream'
data InterpErr e
  = -- | When extent shorthands have not been previously eliminated
    InterpErrShort
  | -- | Wraps custom errors
    InterpErrCustom !e
  deriving stock (Eq, Ord, Show)

instance (Show e, Typeable e) => Exception (InterpErr e)

castInterpErr :: InterpErr Void -> InterpErr e
castInterpErr = \case InterpErrShort -> InterpErrShort

goInterp
  :: (Pattern f)
  => (a -> Either e (f c))
  -> PatF b a (f c, Rational)
  -> Rw b (InterpErr e) (f c, Rational)
goInterp use = \case
  PatPure a -> either (throwRw . InterpErrCustom) (pure . (,1)) (use a)
  PatSilence -> pure (patEmpty, 1)
  PatShort _ -> throwRw InterpErrShort
  PatGroup (Group _ ty els) -> do
    case ty of
      GroupTypeSeq _ -> pure (patSeq els, 1)
      GroupTypePar -> pure (patPar (fmap fst els), 1)
      GroupTypeRand ->
        let els'' = fmap (\(el, w) -> patFastBy w el) els
            s = patRand els''
        in  pure (s, 1)
      GroupTypeAlt ->
        let els'' = fmap (\(el, w) -> patFastBy w el) els
            s = patAlt els''
        in  pure (s, 1)
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
        let el' = patEuc euc el
            w' = fromInteger (eucSteps euc)
        pure (el', w')
      ModTypeElongate (Elongate f) -> do
        let w' = factorValue f * w
        pure (el, w')
      ModTypeReplicate (Replicate mi) -> do
        let v = maybe 2 fromInteger mi
            el' = patRep v el
            w' = fromIntegral v
        pure (el', w')
  PatPoly (Poly _ _) -> error "TODO"

customInterpPat :: (Pattern f) => (a -> Either e (f c)) -> Pat b a -> Either (AnnoErr b (InterpErr e)) (f c)
customInterpPat use = fmap fst . patRw (goInterp use)

recInterpPat :: (Pattern f) => Pat b a -> Either (AnnoErr b (InterpErr e)) (f a)
recInterpPat = either (\(AnnoErr b e) -> Left (AnnoErr b (castInterpErr e))) Right . interpPat

interpPat :: (Pattern f) => Pat b a -> Either (AnnoErr b (InterpErr Void)) (f a)
interpPat = customInterpPat (Right . patPure)
