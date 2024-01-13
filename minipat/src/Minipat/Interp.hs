module Minipat.Interp
  ( InterpErr (..)
  , interpPat
  )
where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception)
import Data.Foldable (foldMap', foldl')
import Data.Foldable1 (foldl1')
import Data.Sequence.NonEmpty qualified as NESeq
import Minipat.Ast qualified as A
import Minipat.Base qualified as B
import Minipat.Norm qualified as N
import Minipat.Proc qualified as P
import Minipat.Rand qualified as R

data InterpErr = InterpErrTime
  deriving stock (Eq, Ord, Show)

instance Exception InterpErr

type M b = P.ProcM InterpErr b (N.Expansion b)

lookInterpPat :: N.NPatX b a (B.Pat a, Rational) -> M b (B.Pat a, Rational)
lookInterpPat = \case
  A.PatPure a -> pure (pure a, 1)
  A.PatSilence -> pure (empty, 1)
  A.PatTime _ -> P.throwPM InterpErrTime
  A.PatGroup (A.Group _ ty els) -> pure $
    case ty of
      A.GroupTypeSeq _ -> foldl1' (\(pout, wout) (pin, win) -> (pout <> B.patLateBy wout pin, wout + win)) els
      A.GroupTypePar -> undefined -- pure (foldl1' (<|>) els)
      A.GroupTypeRand ->
        let l = NESeq.length els
            f arc' =
              let s = R.arcSeed arc'
                  i = R.randInt l s
                  (el, w) = NESeq.index els i
              in  B.unPat (B.patFastBy w el) arc'
        in  (B.Pat (foldMap' (f . B.spanActive . snd) . B.spanSplit), 1)
      A.GroupTypeAlt ->
        let l = NESeq.length els
            f z arc' =
              let i = mod (fromInteger z) l
                  (el, w) = NESeq.index els i
              in  B.unPat (B.patFastBy w el) arc'
        in  (B.Pat (foldMap' (\(z, sp) -> f z (B.spanActive sp)) . B.spanSplit), 1)
  _ -> undefined

repeatPat :: Integer -> B.Pat a -> Rational -> (B.Pat a, Rational)
repeatPat reps midPat width = (finalPat, fromInteger reps * width) where
  finalPat = go 0 0 midPat
  go !w !r !p =
    if r < reps
      then go (w + width) (r + 1) (p <> B.patLateBy w p)
      else p

repInterpPat :: N.NPatX b a (B.Pat a, Rational) -> M b (B.Pat a, Rational)
repInterpPat initPat = do
  reps <- P.asksPM (N.measReps . N.expMeasure)
  (midPat, width) <- lookInterpPat initPat
  pure (repeatPat reps midPat width)

interpPat :: N.NPat b a -> Either (P.ProcErr InterpErr b) (B.Pat a)
interpPat = fmap (\(p', w) -> B.patFastBy w p') . P.bottomUpPM N.expInfo repInterpPat
