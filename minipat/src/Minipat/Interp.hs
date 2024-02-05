{-# LANGUAGE OverloadedStrings #-}

-- | Interpreting patterns as streams
module Minipat.Interp
  ( InterpErr (..)
  , interpPat
  )
where

import Control.Exception (Exception)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Trans (lift)
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
  , PatX
  , Pattern (..)
  , Poly (..)
  , Replicate (..)
  , Speed (..)
  , SpeedDir (..)
  , factorValue
  )
import Minipat.Rewrite (RwErr, RwT, rewriteM, throwRw)

-- | An error interpreting a 'Pat' as a 'Stream'
data InterpErr
  = -- | When extent shorthands have not been previously eliminated
    InterpErrShort
  deriving stock (Eq, Ord, Show)

instance Exception InterpErr

type M b = Except (RwErr InterpErr b)

runM :: M b a -> Either (RwErr InterpErr b) a
runM = runExcept

lookInterp
  :: (Pattern f)
  => PatX b a (M b (f a, Rational))
  -> RwT b (M b) (f a, Rational)
lookInterp = \case
  PatPure a -> pure (patPure a, 1)
  PatSilence -> pure (patEmpty, 1)
  PatShort _ -> throwRw InterpErrShort
  PatGroup (Group _ ty els) -> do
    els' <- lift (sequenceA els)
    case ty of
      GroupTypeSeq _ -> pure (patSeq els', 1)
      GroupTypePar -> pure (patPar (fmap fst els'), 1)
      GroupTypeRand ->
        let els'' = fmap (\(el, w) -> patFastBy w el) els'
            s = patRand els''
        in  pure (s, 1)
      GroupTypeAlt ->
        let els'' = fmap (\(el, w) -> patFastBy w el) els'
            s = patAlt els''
        in  pure (s, 1)
  PatMod (Mod melw md) -> do
    (el, w) <- lift melw
    case md of
      ModTypeSpeed (Speed dir spat) -> do
        spat' <- lift (subInterp spat)
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

subInterp :: (Pattern f) => Pat b a -> M b (f a)
subInterp = fmap fst . rewriteM lookInterp . unPat

-- | Interpret the given 'Pat' as any 'Pattern'
interpPat :: (Pattern f) => Pat b a -> Either (RwErr InterpErr b) (f a)
interpPat = runM . subInterp
