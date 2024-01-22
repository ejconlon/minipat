module Minipat.Interp
  ( Sel
  , SelFn
  , InterpErr (..)
  , interpPat
  )
where

import Bowtie (Anno (..))
import Control.Applicative (Alternative (..))
import Control.Exception (Exception)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Data.Foldable (foldMap')
import Data.Foldable1 (foldl1')
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty qualified as NESeq
import Minipat.Ast qualified as A
import Minipat.Base qualified as B
import Minipat.Rand qualified as D
import Minipat.Rewrite qualified as R

type Sel = Anno (Seq A.Select)

type SelFn = Sel A.Factor -> A.Factor

data InterpErr = InterpErrShort
  deriving stock (Eq, Ord, Show)

instance Exception InterpErr

type M b = ReaderT (Seq A.Select) (Except (R.RwErr InterpErr b))

runM :: M b a -> Either (R.RwErr InterpErr b) a
runM = runExcept . flip runReaderT Empty

lookInterp
  :: SelFn
  -> A.PatX b a (M b (B.Pat (Sel a), Rational))
  -> R.RwT b (M b) (B.Pat (Sel a), Rational)
lookInterp g = \case
  A.PatPure a -> pure (pure (Anno Empty a), 1)
  A.PatSilence -> pure (empty, 1)
  A.PatTime t ->
    case t of
      A.TimeShort _ -> R.throwRw InterpErrShort
      A.TimeLong melw u -> do
        (el, w) <- lift melw
        case u of
          A.LongTimeElongate f -> pure (el, A.factorValue f * w)
          A.LongTimeReplicate mf ->
            let v = maybe 2 fromInteger mf
            in  pure (B.patConcat (NESeq.replicate v (el, 1)), fromIntegral v)
  A.PatGroup (A.Group _ ty els) -> do
    els' <- lift (sequenceA els)
    case ty of
      A.GroupTypeSeq _ -> pure (B.patConcat els', 1)
      A.GroupTypePar -> pure (foldl1' (<|>) (fmap fst els'), 1)
      A.GroupTypeRand ->
        let l = NESeq.length els
            f arc' =
              let s = D.arcSeed arc'
                  i = D.randInt l s
                  (el, w) = NESeq.index els' i
              in  B.unPat (B.patFastBy w el) arc'
        in  pure (B.Pat (foldMap' (f . B.spanActive . snd) . B.spanSplit), 1)
      A.GroupTypeAlt ->
        let l = NESeq.length els
            f z arc' =
              let i = mod (fromInteger z) l
                  (el, w) = NESeq.index els' i
              in  B.unPat (B.patFastBy w el) arc'
        in  pure (B.Pat (foldMap' (\(z, sp) -> f z (B.spanActive sp)) . B.spanSplit), 1)
  A.PatMod (A.Mod mx md) -> do
    (r', w) <- lift mx
    case md of
      A.ModTypeSpeed (A.Speed dir spat) -> do
        spat' <- lift (subInterp g spat)
        let f = case dir of
              A.SpeedDirFast -> B.patFast
              A.SpeedDirSlow -> B.patSlow
            spat'' = fmap (A.factorValue . g) spat'
        pure (f spat'' r', w)
      A.ModTypeSelect _ -> error "TODO"
      A.ModTypeDegrade _ -> error "TODO"
      A.ModTypeEuclid _ -> error "TODO"
  A.PatPoly (A.PolyPat _ _) -> error "TODO"

subInterp :: SelFn -> A.Pat b a -> M b (B.Pat (Sel a))
subInterp g = fmap fst . R.rewriteM (lookInterp g) . A.unPat

interpPat' :: SelFn -> A.Pat b a -> Either (R.RwErr InterpErr b) (B.Pat (Sel a))
interpPat' g = runM . subInterp g

interpPat :: A.Pat b a -> Either (R.RwErr InterpErr b) (B.Pat (Sel a))
interpPat = interpPat' annoVal
