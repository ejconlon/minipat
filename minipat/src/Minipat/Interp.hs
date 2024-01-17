module Minipat.Interp
  ( InterpErr (..)
  , interpPat
  )
where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Trans (lift)
import Data.Foldable (foldMap')
import Data.Foldable1 (foldl1')
import Data.Sequence.NonEmpty qualified as NESeq
import Minipat.Ast qualified as A
import Minipat.Base qualified as B
import Minipat.Rand qualified as D
import Minipat.Rewrite qualified as R

data InterpErr = InterpErrShort
  deriving stock (Eq, Ord, Show)

instance Exception InterpErr

type M b = Except (R.RwErr InterpErr b)

runM :: M b a -> Either (R.RwErr InterpErr b) a
runM = runExcept

lookInterp :: A.PatX b a (M b (B.Pat a, Rational)) -> R.RwT b (M b) (B.Pat a, Rational)
lookInterp = \case
  A.PatPure a -> pure (pure a, 1)
  A.PatSilence -> pure (empty, 1)
  A.PatTime t ->
    case t of
      A.TimeShort _ -> R.throwRw InterpErrShort
      _ -> error "TODO"
  A.PatGroup (A.Group _ ty els) -> do
    els' <- lift (sequenceA els)
    pure $ case ty of
      A.GroupTypeSeq _ ->
        foldl1' (\(pout, wout) (pin, win) -> (pout <> B.patLateBy wout pin, wout + win)) els'
      A.GroupTypePar -> (foldl1' (<|>) (fmap fst els'), 1)
      A.GroupTypeRand ->
        let l = NESeq.length els
            f arc' =
              let s = D.arcSeed arc'
                  i = D.randInt l s
                  (el, w) = NESeq.index els' i
              in  B.unPat (B.patFastBy w el) arc'
        in  (B.Pat (foldMap' (f . B.spanActive . snd) . B.spanSplit), 1)
      A.GroupTypeAlt ->
        let l = NESeq.length els
            f z arc' =
              let i = mod (fromInteger z) l
                  (el, w) = NESeq.index els' i
              in  B.unPat (B.patFastBy w el) arc'
        in  (B.Pat (foldMap' (\(z, sp) -> f z (B.spanActive sp)) . B.spanSplit), 1)
  A.PatMod (A.Mod mx md) -> do
    (r', _) <- lift mx
    case md of
      A.ModTypeSpeed (A.Speed dir spat) -> do
        spat' <- lift (subInterp spat)
        let f = case dir of
              A.SpeedDirFast -> B.patFast
              A.SpeedDirSlow -> B.patSlow
        pure (f (fmap A.factorValue spat') r', 1)
      _ -> error "TODO"
  _ -> error "TODO"

subInterp :: A.Pat b a -> M b (B.Pat a)
subInterp = fmap (\(p', w) -> B.patFastBy w p') . R.rewriteM lookInterp . A.unPat

interpPat :: A.Pat b a -> Either (R.RwErr InterpErr b) (B.Pat a)
interpPat = runM . subInterp
