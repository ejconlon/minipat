module Minipat.Norm where

-- TODO explicit exports

import Bowtie (pattern JotP)
import Control.Exception (Exception)
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty (NESeq (..))
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Typeable (Typeable)
import Minipat.Ast qualified as A

-- data NormErr = NormErr
--   deriving stock (Eq, Ord, Show)
--
-- instance Exception NormErr

-- * Norm

-- | Error from expression normalization
newtype NormErr b = NormErr (NESeq b)
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance (Show b, Typeable b) => Exception (NormErr b)

-- | When normalizing, we maintain the path of spans to the term
-- through this monad.
type NormM b = ReaderT (NESeq b) (Except (NormErr b))

runNormM :: NormM b a -> b -> Either (NormErr b) a
runNormM ma b = runExcept (runReaderT ma (NESeq.singleton b))

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

-- ** Normalization

type NPat b = A.Pat (Expansion b)

type NPatX b = A.PatF (NPat b A.Factor)

type UnNPat b = A.UnPat (Expansion b)

type NPatK b a = NPatX b a (UnNPat b a)

wrapPatM :: NPatK b a -> NormM b (NPat b a)
wrapPatM ff = asks (\(b :<|| _) -> A.Pat (JotP (expNew b) ff))

rewrapPatM :: (Semigroup b) => A.UnPat b a -> (UnNPat b a -> NormM b (NPatK b a)) -> NormM b (NPat b a)
rewrapPatM r f = do
  p <- normPatM (A.Pat r)
  let A.Pat r'@(JotP ex _) = p
  k <- f r'
  asks (\(b :<|| _) -> A.Pat (JotP (ex {expInfo = b}) k))

normModPatM :: (Semigroup b) => A.ModPat (A.Pat b a) -> NormM b (A.ModPat (NPat b a))
normModPatM = \case
  A.ModPatSelect s -> pure (A.ModPatSelect s)
  A.ModPatSpeed s -> fmap A.ModPatSpeed (traverse normPatM s)
  A.ModPatDegrade d -> pure (A.ModPatDegrade d)
  A.ModPatEuclid e -> pure (A.ModPatEuclid e)

foldNormPatKM :: (Semigroup b) => Int -> A.GroupPatType -> NESeq (A.UnPat b a) -> NormM b (NPat b a)
foldNormPatKM lvl ty = goFirst
 where
  goFirst (y :<|| ys) = do
    A.Pat w <- normPatM (A.Pat y)
    goRest (NESeq.singleton w) ys
  goRest ws@(wsi :||> JotP (Expansion (Measure xi yi) vi) ffi) = \case
    Empty -> wrapPatM (A.PatGroup (A.GroupPat lvl ty ws))
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
          A.Pat w <- local (b NESeq.<|) (normPatKM ff)
          pure (ws NESeq.|> w)
      goRest ws' ys

normPatKM :: (Semigroup b) => A.PatK b a -> NormM b (NPat b a)
normPatKM = \case
  A.PatPure a -> wrapPatM (A.PatPure a)
  A.PatSilence -> wrapPatM A.PatSilence
  A.PatTime t ->
    case t of
      -- Time shorthands at top level are nonsense - throw error
      A.TimeShort _ -> ask >>= throwError . NormErr
      -- Annotated time expressions turn into decorations
      A.TimeLong r l -> do
        p <- normPatM (A.Pat r)
        let A.Pat (JotP (Expansion (Measure xi yi) vi) ff') = p
        let m = case l of
              A.LongTimeElongate f -> Measure xi (yi * A.factorValue f)
              A.LongTimeReplicate mi -> Measure (maybe (xi + 1) (xi *) mi) yi
        asks $ \(b :<|| _) ->
          let v = vi <> b
          in  A.Pat (JotP (Expansion m v) ff')
  A.PatGroup (A.GroupPat lvl ty ss) ->
    case ss of
      -- Unwrap any singletons we find
      q :<|| Empty -> normPatM (A.Pat q)
      -- Otherwise normalize by folding
      _ -> foldNormPatKM lvl ty ss
  A.PatMod (A.Mod r m) ->
    -- Just propagate time controls upward
    rewrapPatM r $ \r' -> do
      m' <- normModPatM m
      pure (A.PatMod (A.Mod r' m'))
  A.PatPoly (A.PolyPat rs mc) -> do
    -- Just recurse and reset time controls here
    rs' <- traverse (fmap A.unPat . normPatM . A.Pat) rs
    wrapPatM (A.PatPoly (A.PolyPat rs' mc))

normPatM :: (Semigroup b) => A.Pat b a -> NormM b (NPat b a)
normPatM (A.Pat (JotP b ff)) = local (b NESeq.<|) (normPatKM ff)

normPat :: (Semigroup b) => A.Pat b a -> Either (NormErr b) (NPat b a)
normPat (A.Pat (JotP b ff)) = runNormM (normPatKM ff) b
