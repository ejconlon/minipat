module Minipat.Interp
  ( InterpErr (..)
  , interpPat
  )
where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception)
import Control.Monad.Except (Except, ExceptT, runExcept, runExceptT)
import Control.Monad.Trans (lift)
import Data.Foldable (foldMap')
import Data.Foldable1 (foldMap1', foldl1')
import Data.Semigroup (Sum (..))
import Data.Sequence.NonEmpty qualified as NESeq
import Minipat.Ast qualified as A
import Minipat.Base qualified as B
import Minipat.Rand qualified as D
import Minipat.Rewrite qualified as R

data InterpErr = InterpErrShort
  deriving stock (Eq, Ord, Show)

instance Exception InterpErr

type M b = ExceptT (R.RwErr InterpErr b) IO

runM :: M b a -> IO (Either (R.RwErr InterpErr b) a)
runM = runExceptT

lookInterp :: A.PatX b a (M b (B.Pat a, Rational)) -> R.RwT b (M b) (B.Pat a, Rational)
lookInterp = \case
  A.PatPure a -> pure (pure a, 1)
  A.PatSilence -> pure (empty, 1)
  A.PatTime t ->
    case t of
      A.TimeShort _ -> R.throwRw InterpErrShort
      A.TimeLong melw t -> do
        (el, w) <- lift melw
        case t of
          A.LongTimeElongate f -> pure (el, A.factorValue f * w)
          A.LongTimeReplicate mf ->
            let v = maybe 2 fromInteger mf
            in pure (B.patConcat (NESeq.replicate v (el, 1)), fromIntegral v)
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
        spat' <- lift (subInterp spat)
        let f = case dir of
              A.SpeedDirFast -> B.patFast
              A.SpeedDirSlow -> B.patSlow
        pure (f (fmap A.factorValue spat') r', w)
      _ -> error "TODO"
  _ -> error "TODO"

subInterp :: A.Pat b a -> M b (B.Pat a)
subInterp = fmap fst . R.rewriteM lookInterp . A.unPat

interpPat :: A.Pat b a -> IO (Either (R.RwErr InterpErr b) (B.Pat a))
interpPat = runM . subInterp
