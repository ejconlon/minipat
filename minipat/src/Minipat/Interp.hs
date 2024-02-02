{-# LANGUAGE OverloadedStrings #-}

-- | Interpreting patterns as streams
module Minipat.Interp
  ( SelFn
  , noSelFn
  , SelAcc
  , accSelFn
  , accSelProj
  , InterpErr (..)
  , interpPat
  , interpPatAccSel
  , interpPatNoSel
  )
where

import Bowtie (Anno (..))
import Control.Exception (Exception)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Trans (lift)
import Data.Foldable (foldMap')
import Data.Foldable1 (foldl1')
import Data.Ratio ((%))
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Typeable (Typeable)
import Data.Void (Void)
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
import Minipat.Rand qualified as D
import Minipat.Rewrite (RwErr, RwT, rewriteM, throwRw)
import Minipat.Stream
  ( Stream (..)
  , streamConcat
  , streamDegradeBy
  , streamFast
  , streamFastBy
  , streamReplicate
  , streamSlow
  )
import Minipat.Time (Cycle (..), CycleDelta (..), spanActive, spanSplit)

-- | A function that processes a 'Select'
type SelFn e a = Select -> Stream a -> Either (InterpErr e) (Stream a)

-- | Error for when we encounter selects when forbidden
data NoSelectErr = NoSelectErr
  deriving stock (Eq, Ord, Show)

instance Exception NoSelectErr

-- | A function forbidding any selects
noSelFn :: SelFn e a
noSelFn _ _ = Left InterpErrForbidden

-- | An accumulation of selects (higher on tree in the front)
type SelAcc = Anno (Seq Select)

-- | A function accumulating selects
accSelFn :: SelFn e (SelAcc a)
accSelFn sel = Right . fmap (\(Anno sels a) -> Anno (sel :<| sels) a)

-- | Projection into select accumulator
accSelProj :: a -> SelAcc a
accSelProj = Anno Empty

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
  :: SelFn e c
  -> (a -> c)
  -> PatX b a (M b e (Stream c, CycleDelta))
  -> RwT b (M b e) (Stream c, CycleDelta)
lookInterp selFn projFn = \case
  PatPure a -> pure (pure (projFn a), 1)
  PatSilence -> pure (mempty, 1)
  PatExtent t ->
    case t of
      ExtentShort _ -> throwRw InterpErrShort
      ExtentLong melw u -> do
        (el, w) <- lift melw
        case u of
          LongExtentElongate f -> pure (el, CycleDelta (factorValue f * unCycleDelta w))
          LongExtentReplicate mf ->
            let v = maybe 2 fromInteger mf
            in  pure (streamReplicate v el 1, fromIntegral v)
  PatGroup (Group _ ty els) -> do
    els' <- lift (sequenceA els)
    case ty of
      GroupTypeSeq _ -> pure (streamConcat els', 1)
      GroupTypePar -> pure (foldl1' (<>) (fmap fst els'), 1)
      GroupTypeRand ->
        let l = NESeq.length els
            f arc' =
              let s = D.arcSeed arc'
                  i = D.randInt l s
                  (el, w) = NESeq.index els' i
              in  unStream (streamFastBy (unCycleDelta w) el) arc'
        in  pure (Stream (foldMap' (f . spanActive . snd) . spanSplit), 1)
      GroupTypeAlt ->
        let l = NESeq.length els
            f z arc' =
              let i = mod (fromInteger (unCycle z)) l
                  (el, w) = NESeq.index els' i
              in  unStream (streamFastBy (unCycleDelta w) el) arc'
        in  pure (Stream (foldMap' (\(z, sp) -> f z (spanActive sp)) . spanSplit), 1)
  PatMod (Mod melw md) ->
    case md of
      ModTypeSpeed (Speed dir spat) -> do
        spat' <- lift (subInterp noSelFn id spat)
        let f = case dir of
              SpeedDirFast -> streamFast
              SpeedDirSlow -> streamSlow
            spat'' = fmap factorValue spat'
        (el, w) <- lift melw
        pure (f spat'' el, w)
      ModTypeSelect s -> do
        (el, w) <- lift melw
        case selFn s el of
          Left err -> throwRw err
          Right el' -> pure (el', w)
      ModTypeDegrade (Degrade dd) -> do
        let d = maybe (1 % 2) factorValue dd
        (el, w) <- lift melw
        let el' = streamDegradeBy d el
        pure (el', w)
      ModTypeEuclid euc -> do
        (el, _) <- lift melw
        let s = streamConcat (eucSeq euc (el, 1) (mempty, 1))
            d = eucSteps euc
        pure (s, fromInteger d)
  PatPoly (Poly _ _) -> error "TODO"

eucSeq :: Euclid -> r -> r -> NESeq r
eucSeq (Euclid (fromInteger -> filled) (fromInteger -> steps) (maybe 0 fromInteger -> shift)) activeEl passiveEl =
  NESeq.fromFunction steps $ \ix0 ->
    let ix1 = ix0 + shift
        ix = if ix1 >= steps then ix1 - steps else ix1
        active = mod ix filled == 0
    in  if active then activeEl else passiveEl

subInterp :: SelFn e c -> (a -> c) -> Pat b a -> M b e (Stream c)
subInterp selC projAC = fmap fst . rewriteM (lookInterp selC projAC) . unPat

-- | Interpret the given 'Pat' as a 'Stream'
interpPat :: SelFn e c -> (a -> c) -> Pat b a -> Either (RwErr (InterpErr e) b) (Stream c)
interpPat selC projAC = runM . subInterp selC projAC

-- | 'interpPat' acumulating selects
interpPatAccSel :: Pat b a -> Either (RwErr (InterpErr Void) b) (Stream (SelAcc a))
interpPatAccSel = interpPat accSelFn accSelProj

-- | 'interpPat' forbidding selects
interpPatNoSel :: Pat b a -> Either (RwErr (InterpErr Void) b) (Stream a)
interpPatNoSel = interpPat noSelFn id
