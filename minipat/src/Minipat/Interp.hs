{-# LANGUAGE OverloadedStrings #-}

-- | Interpreting patterns as streams
module Minipat.Interp
  ( InterpErr (..)
  , interpPat
  )
where

import Bowtie.Rewrite (AnnoErr, Rw, embedRw, throwRw)
import Control.Exception (Exception)
import Data.Ratio ((%))
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
data InterpErr
  = -- | When extent shorthands have not been previously eliminated
    InterpErrShort
  deriving stock (Eq, Ord, Show)

instance Exception InterpErr

lookInterp
  :: (Pattern f)
  => PatF b a (f a, Rational)
  -> Rw b InterpErr (f a, Rational)
lookInterp = \case
  PatPure a -> pure (patPure a, 1)
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
        spat' <- embedRw (interpPat spat)
        let f = case dir of
              SpeedDirFast -> patFast
              SpeedDirSlow -> patSlow
            spat'' = fmap factorValue spat'
            el' = f spat'' el
        pure (el', w)
      ModTypeDegrade (Degrade dd) -> do
        let d = maybe (1 % 2) factorValue dd
        let el' = patDegBy d el
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

interpPat :: (Pattern f) => Pat b a -> Either (AnnoErr b InterpErr) (f a)
interpPat = fmap fst . patRw lookInterp
