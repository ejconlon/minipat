module Minipat.Norm
  ( Measure (..)
  , measNew
  , Expansion (..)
  , expNew
  , NPat
  , NPatX
  , normPat
  )
where

import Bowtie (pattern JotP)
import Control.Exception (Exception)
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty (NESeq (..))
import Data.Sequence.NonEmpty qualified as NESeq
import Minipat.Ast qualified as A
import Minipat.Proc qualified as P

data Measure = Measure
  { measReps :: !Integer
  -- ^ Repetitions
  , measWidth :: !Rational
  -- ^ Width factor
  }
  deriving (Eq, Ord, Show)

measNew :: Measure
measNew = Measure 1 1

measTotal :: Measure -> Rational
measTotal (Measure reps width) = fromInteger reps * width

-- | Decoration for normalized expressions that includes measure.
data Expansion b = Expansion
  { expMeasure :: !Measure
  -- ^ Measure of repetitions and width
  , expInfo :: !b
  -- ^ Original info
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

expNew :: b -> Expansion b
expNew = Expansion measNew

-- | Error from expression normalization
data NormErr = NormErr
  deriving stock (Eq, Ord, Show)

instance Exception NormErr

-- | When normalizing, we maintain the path of spans to the term
-- through this monad.
type NormM b = P.ProcM NormErr b b

runNormM :: NormM b a -> b -> Either (P.ProcErr NormErr b) a
runNormM ma = P.runPM ma id

-- ** Normalization

type NPat b = A.Pat (Expansion b)

type NPatX b = A.PatF (NPat b A.Factor)

type UnNPat b = A.UnPat (Expansion b)

type NPatK b a = NPatX b a (UnNPat b a)

wrapPatM :: NPatK b a -> NormM b (NPat b a)
wrapPatM ff = P.asksPM (\b -> A.Pat (JotP (expNew b) ff))

rewrapPatM :: (Semigroup b) => A.UnPat b a -> (UnNPat b a -> NormM b (NPatK b a)) -> NormM b (NPat b a)
rewrapPatM r f = do
  p <- normPatM (A.Pat r)
  let A.Pat r'@(JotP ex _) = p
  ff <- f r'
  P.asksPM (\b -> A.Pat (JotP (ex {expInfo = b}) ff))

normModTypeM :: (Semigroup b) => A.ModType (A.Pat b a) -> NormM b (A.ModType (NPat b a))
normModTypeM = \case
  A.ModTypeSelect s -> pure (A.ModTypeSelect s)
  A.ModTypeSpeed s -> fmap A.ModTypeSpeed (traverse normPatM s)
  A.ModTypeDegrade d -> pure (A.ModTypeDegrade d)
  A.ModTypeEuclid e -> pure (A.ModTypeEuclid e)

foldNormPatKM :: (Semigroup b) => Int -> A.GroupType -> NESeq (A.UnPat b a) -> NormM b (NPat b a)
foldNormPatKM lvl ty = goFirst
 where
  goFirst (y :<|| ys) = do
    A.Pat w <- normPatM (A.Pat y)
    goRest (NESeq.singleton w) ys
  goRest ws@(wsi :||> JotP (Expansion (Measure xi yi) vi) ffi) = \case
    Empty -> wrapPatM (A.PatGroup (A.Group lvl ty ws))
    JotP b ff :<| ys -> do
      ws' <- case ff of
        A.PatTime (A.TimeShort s) -> do
          let m = case s of
                A.ShortTimeElongate -> Measure xi (yi + 1)
                A.ShortTimeReplicate -> Measure (xi + 1) yi
              v = vi <> b
              w = JotP (Expansion m v) ffi
          pure (wsi :||> w)
        _ -> do
          A.Pat w <- P.pushPM b (normPatKM ff)
          pure (ws NESeq.|> w)
      goRest ws' ys

normPatKM :: (Semigroup b) => A.PatK b a -> NormM b (NPat b a)
normPatKM = \case
  A.PatPure a -> wrapPatM (A.PatPure a)
  A.PatSilence -> wrapPatM A.PatSilence
  A.PatTime t ->
    case t of
      -- Time shorthands at top level are nonsense - throw error
      A.TimeShort _ -> P.throwPM NormErr
      -- Annotated time expressions turn into decorations
      A.TimeLong r l -> do
        p <- normPatM (A.Pat r)
        let A.Pat (JotP (Expansion (Measure xi yi) vi) ff') = p
        let m = case l of
              A.LongTimeElongate f -> Measure xi (yi * A.factorValue f)
              A.LongTimeReplicate mi -> Measure (maybe (xi + 1) (xi *) mi) yi
        b <- P.askPM
        let v = vi <> b
        pure (A.Pat (JotP (Expansion m v) ff'))
  A.PatGroup (A.Group lvl ty ss) ->
    case ss of
      -- Unwrap any singletons we find
      q :<|| Empty -> normPatM (A.Pat q)
      -- Otherwise normalize by folding
      _ -> foldNormPatKM lvl ty ss
  A.PatMod (A.Mod r m) ->
    -- Just propagate time controls upward
    rewrapPatM r $ \r' -> do
      m' <- normModTypeM m
      pure (A.PatMod (A.Mod r' m'))
  A.PatPoly (A.PolyPat rs mc) -> do
    -- Just recurse and reset time controls here
    rs' <- traverse (fmap A.unPat . normPatM . A.Pat) rs
    wrapPatM (A.PatPoly (A.PolyPat rs' mc))

normPatM :: (Semigroup b) => A.Pat b a -> NormM b (NPat b a)
normPatM (A.Pat (JotP b ff)) = P.pushPM b (normPatKM ff)

normPat :: (Semigroup b) => A.Pat b a -> Either (P.ProcErr NormErr b) (NPat b a)
normPat (A.Pat (JotP b ff)) = runNormM (normPatKM ff) b
