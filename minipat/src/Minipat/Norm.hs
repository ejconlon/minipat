module Minipat.Norm where

-- TODO explicit exports

import Bowtie (pattern JotP)
import Control.Exception (Exception)
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty (NESeq (..))
import Data.Sequence.NonEmpty qualified as NESeq
import Minipat.Ast qualified as A
import Minipat.Rewrite qualified as R

-- | Error from expression normalization
data NormErr = NormErrShort
  deriving stock (Eq, Ord, Show)

instance Exception NormErr

type M b = R.RwM NormErr b

foldNormPat :: NESeq (A.UnPat b a) -> NESeq (A.UnPat b a)
foldNormPat = goFirst
 where
  goFirst (y :<|| ys) = do
    goRest (NESeq.singleton y) ys
  goRest ws@(winit :||> wlast) = \case
    Empty -> ws
    y@(JotP b pf) :<| ys ->
      let ws' = case pf of
            A.PatTime (A.TimeShort s) ->
              let pf' = A.PatTime $ A.TimeLong wlast $ case s of
                    A.ShortTimeElongate -> A.LongTimeElongate 1
                    A.ShortTimeReplicate -> A.LongTimeReplicate Nothing
              in winit :||> JotP b pf'
            _ -> ws NESeq.|> y
      in goRest ws' ys

normPatM :: A.PatX b a (A.UnPat b a) -> M b (A.UnPat b a)
normPatM = \case
  -- Simple cases first - just rewrap
  A.PatPure a -> R.wrapRw (A.PatPure a)
  A.PatSilence -> R.wrapRw A.PatSilence
  A.PatMod m -> R.wrapRw (A.PatMod m)
  A.PatPoly p -> R.wrapRw (A.PatPoly p)
  -- Handling time expressions
  A.PatTime t ->
    case t of
      -- Time shorthands at top level are nonsense - throw error
      A.TimeShort _ -> R.throwRw NormErrShort
      -- Otherwise rewrap
      A.TimeLong r l -> R.wrapRw (A.PatTime (A.TimeLong r l))
  -- Handling groups
  A.PatGroup (A.Group lvl ty ss) -> do
    let ss' = case ty of
          A.GroupTypeSeq _ -> foldNormPat ss
          _ -> ss
    -- Unwrap any singletons we find
    case ss' of
      q :<|| Empty -> pure q
      _ -> R.wrapRw (A.PatGroup (A.Group lvl ty ss'))

normPat :: A.Pat b a -> Either (R.RwErr NormErr b) (A.Pat b a)
normPat = R.finishRw . R.overhaulM normPatM

