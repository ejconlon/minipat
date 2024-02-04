{-# LANGUAGE OverloadedStrings #-}

-- | Interpreting patterns as streams
module Minipat.Interp
  ( Sel
  , InterpEnv (..)
  , forbidSel
  , forbidInterpEnv
  , SelAcc
  , accSel
  , accProj
  , accInterpEnv
  , InterpErr (..)
  , interpPat
  )
where

import Bowtie (Anno (..))
import Control.Exception (Exception)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Trans (lift)
import Data.Ratio ((%))
import Data.Sequence (Seq (..))
import Data.Typeable (Typeable)
import Minipat.Ast
  ( Degrade (..)
  , Euclid (..)
  , Extent (..)
  , Group (..)
  , GroupType (..)
  , LongExtent (..)
  , Mod (..)
  , ModType (..)
  , Pat (..)
  , PatF (..)
  , PatX
  , Poly (..)
  , Select (..)
  , Speed (..)
  , SpeedDir (..)
  , factorValue
  )
import Minipat.Class (Pattern (..))
import Minipat.Rewrite (RwErr, RwT, rewriteM, throwRw)
import Minipat.Stream
  ( Stream (..)
  , streamDegradeBy
  , streamFast
  , streamFastBy
  , streamSlow
  )
import Minipat.Time (CycleDelta (..))

-- | A function that processes a 'Select'
type Sel e a = Select -> Stream a -> Either (InterpErr e) (Stream a)

data InterpEnv e a c = InterpEnv
  { ieSel :: !(Sel e c)
  , icProj :: !(a -> c)
  }

-- | A function forbidding any selects
forbidSel :: Sel e a
forbidSel _ _ = Left InterpErrForbidden

forbidInterpEnv :: InterpEnv e a a
forbidInterpEnv = InterpEnv forbidSel id

-- | An accumulation of selects (higher on tree in the front)
type SelAcc = Anno (Seq Select)

-- | A function accumulating selects
accSel :: Sel e (SelAcc a)
accSel sel = Right . fmap (\(Anno sels a) -> Anno (sel :<| sels) a)

-- | Projection into select accumulator
accProj :: a -> SelAcc a
accProj = Anno Empty

accInterpEnv :: InterpEnv e a (SelAcc a)
accInterpEnv = InterpEnv accSel accProj

-- | An error interpreting a 'Pat' as a 'Stream'
data InterpErr e
  = -- | When extent shorthands have not been previously eliminated
    InterpErrShort
  | -- | When selects are forbidden
    InterpErrForbidden
  | -- | Custom select errors
    InterpErrEmbed !e
  deriving stock (Eq, Ord, Show)

instance (Show e, Typeable e) => Exception (InterpErr e)

type M b e = Except (RwErr (InterpErr e) b)

runM :: M b e a -> Either (RwErr (InterpErr e) b) a
runM = runExcept

lookInterp
  :: InterpEnv e a c
  -> PatX b a (M b e (Stream c, CycleDelta))
  -> RwT b (M b e) (Stream c, CycleDelta)
lookInterp (InterpEnv sel proj) = \case
  PatPure a -> pure (patPure (proj a), 1)
  PatSilence -> pure (patEmpty, 1)
  PatExtent t ->
    case t of
      ExtentShort _ -> throwRw InterpErrShort
      ExtentLong melw u -> do
        (el, w) <- lift melw
        case u of
          LongExtentElongate f -> pure (el, CycleDelta (factorValue f * unCycleDelta w))
          LongExtentReplicate mf ->
            let v = maybe 2 fromInteger mf
            in  pure (patRep v el, fromIntegral v)
  PatGroup (Group _ ty els) -> do
    els' <- lift (sequenceA els)
    case ty of
      GroupTypeSeq _ -> pure (patSeq els', 1)
      GroupTypePar -> pure (patPar (fmap fst els'), 1)
      GroupTypeRand ->
        let els'' = fmap (\(el, w) -> streamFastBy (unCycleDelta w) el) els'
            s = patRand els''
        in  pure (s, 1)
      GroupTypeAlt ->
        let els'' = fmap (\(el, w) -> streamFastBy (unCycleDelta w) el) els'
            s = patAlt els''
        in  pure (s, 1)
  PatMod (Mod melw md) ->
    case md of
      ModTypeSpeed (Speed dir spat) -> do
        spat' <- lift (subInterp forbidInterpEnv spat)
        let f = case dir of
              SpeedDirFast -> streamFast
              SpeedDirSlow -> streamSlow
            spat'' = fmap factorValue spat'
        (el, w) <- lift melw
        pure (f spat'' el, w)
      ModTypeSelect s -> do
        (el, w) <- lift melw
        case sel s el of
          Left err -> throwRw err
          Right el' -> pure (el', w)
      ModTypeDegrade (Degrade dd) -> do
        let d = maybe (1 % 2) factorValue dd
        (el, w) <- lift melw
        let el' = streamDegradeBy d el
        pure (el', w)
      ModTypeEuclid euc -> do
        let (Euclid (fromInteger -> filled) (fromInteger -> steps) (fmap fromInteger -> mshift)) = euc
        (el, _) <- lift melw
        let s = patEuc filled steps mshift el
        pure (s, fromIntegral steps)
  PatPoly (Poly _ _) -> error "TODO"

subInterp :: InterpEnv e a c -> Pat b a -> M b e (Stream c)
subInterp env = fmap fst . rewriteM (lookInterp env) . unPat

-- | Interpret the given 'Pat' as a 'Stream'
interpPat :: InterpEnv e a c -> Pat b a -> Either (RwErr (InterpErr e) b) (Stream c)
interpPat env = runM . subInterp env
