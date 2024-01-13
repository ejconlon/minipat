module Minipat.Interp
  ( InterpErr (..)
  , interpPat
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

subInterpPat :: N.NPatX b a (B.Pat a) -> M b (B.Pat a)
subInterpPat = \case
  A.PatPure a -> pure (pure a)
  A.PatSilence -> pure empty
  A.PatTime _ -> P.throwPM InterpErrTime
  A.PatGroup (A.Group _ ty els) -> pure $
    case ty of
      A.GroupTypeSeq _ -> fold1 els
      A.GroupTypePar -> foldl1' (<|>) els
      A.GroupTypeRand ->
        let l = NESeq.length els
            f arc' =
              let s = R.arcSeed arc'
                  i = R.randInt l s
                  el = NESeq.index els i
              in  B.unPat el arc'
        in  B.Pat (foldMap' (f . B.spanActive . snd) . B.spanSplit)
      A.GroupTypeAlt ->
        let l = NESeq.length els
            f z arc' =
              let i = mod (fromInteger z) l
                  el = NESeq.index els i
              in  B.unPat el arc'
        in  B.Pat (foldMap' (\(z, sp) -> f z (B.spanActive sp)) . B.spanSplit)
  _ -> undefined

interpPat :: N.NPat b a -> Either (P.ProcErr InterpErr b) (B.Pat a)
interpPat = P.bottomUpPM N.expInfo subInterpPat
