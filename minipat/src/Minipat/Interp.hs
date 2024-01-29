module Minipat.Interp
  ( Sel
  , SelFn
  , yesSelFn
  , noSelFn
  , InterpErr (..)
  , interpPat
  )
where

import Bowtie (Anno (..))
import Control.Applicative (Alternative (..))
import Control.Exception (Exception)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Data.Foldable (foldMap')
import Data.Foldable1 (foldl1')
import Data.Ratio ((%))
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty qualified as NESeq
import Minipat.Ast qualified as A
import Minipat.Base qualified as B
import Minipat.Rand qualified as D
import Minipat.Rewrite qualified as R
import Minipat.Time qualified as T

type Sel = Anno (Seq A.Select)

type SelFn a c = Seq A.Select -> a -> Maybe c

yesSelFn :: SelFn a (Sel a)
yesSelFn ss = Just . Anno ss

noSelFn :: SelFn a a
noSelFn = \case
  Empty -> Just
  _ -> const Nothing

data InterpErr
  = InterpErrShort
  | InterpErrSel
  deriving stock (Eq, Ord, Show)

instance Exception InterpErr

type M b = ReaderT (Seq A.Select) (Except (R.RwErr InterpErr b))

runM :: M b a -> Either (R.RwErr InterpErr b) a
runM = runExcept . flip runReaderT Empty

lookInterp
  :: SelFn A.Factor A.Factor
  -> SelFn a c
  -> A.PatX b a (M b (B.Pat c, Rational))
  -> R.RwT b (M b) (B.Pat c, Rational)
lookInterp g h = \case
  A.PatPure a -> do
    ss <- lift ask
    maybe (R.throwRw InterpErrSel) (\c -> pure (pure c, 1)) (h ss a)
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
        in  pure (B.Pat (foldMap' (f . T.spanActive . snd) . T.spanSplit), 1)
      A.GroupTypeAlt ->
        let l = NESeq.length els
            f z arc' =
              let i = mod (fromInteger z) l
                  (el, w) = NESeq.index els' i
              in  B.unPat (B.patFastBy w el) arc'
        in  pure (B.Pat (foldMap' (\(z, sp) -> f z (T.spanActive sp)) . T.spanSplit), 1)
  A.PatMod (A.Mod mx md) -> do
    case md of
      A.ModTypeSpeed (A.Speed dir spat) -> do
        spat' <- lift (subInterp g g spat)
        let f = case dir of
              A.SpeedDirFast -> B.patFast
              A.SpeedDirSlow -> B.patSlow
            spat'' = fmap A.factorValue spat'
        (r', w) <- lift mx
        pure (f spat'' r', w)
      A.ModTypeSelect s -> lift (local (:|> s) mx)
      A.ModTypeDegrade (A.Degrade dd) -> do
        let d = maybe (1 % 2) A.factorValue dd
        (r', w) <- lift mx
        let r'' = B.patDegradeBy d r'
        pure (r'', w)
      A.ModTypeEuclid _ -> error "TODO"
  A.PatPoly (A.PolyPat _ _) -> error "TODO"

subInterp :: SelFn A.Factor A.Factor -> SelFn a c -> A.Pat b a -> M b (B.Pat c)
subInterp g h = fmap fst . R.rewriteM (lookInterp g h) . A.unPat

interpPat :: SelFn A.Factor A.Factor -> SelFn a c -> A.Pat b a -> Either (R.RwErr InterpErr b) (B.Pat c)
interpPat g h = runM . subInterp g h
