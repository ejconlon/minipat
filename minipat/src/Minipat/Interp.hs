module Minipat.Interp
  ( InterpErr (..)
  , interp
  )
where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception)
import Data.Foldable (foldMap')
import Data.Foldable1 (fold1, foldl1')
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

subInterp :: N.NPatX b a (B.Pat a) -> M b (B.Pat a)
subInterp = \case
  A.PatPure a -> pure (pure a)
  A.PatSilence -> pure empty
  A.PatTime _ -> P.throwPM InterpErrTime
  A.PatGroup (A.GroupPat _ ty els) -> pure $
    case ty of
      A.GroupPatTypeSeq _ -> fold1 els
      A.GroupPatTypePar -> foldl1' (<|>) els
      A.GroupPatTypeRand ->
        let l = NESeq.length els
            f arc' =
              let s = R.arcSeed arc'
                  i = R.randInt l s
                  el = NESeq.index els i
              in  B.unPat el arc'
        in  B.Pat (foldMap' (f . B.spanActive . snd) . B.spanSplit)
      A.GroupPatTypeAlt ->
        let l = NESeq.length els
            f z arc' =
              let i = mod (fromInteger z) l
                  el = NESeq.index els i
              in  B.unPat el arc'
        in  B.Pat (foldMap' (\(z, sp) -> f z (B.spanActive sp)) . B.spanSplit)
  _ -> undefined

interp :: N.NPat b a -> Either (P.ProcErr InterpErr b) (B.Pat a)
interp = P.bottomUpPM N.expInfo subInterp
