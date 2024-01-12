{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Minipat.Ast where

import Bowtie (Jot (..), pattern JotP)
import Control.Exception (Exception)
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Foldable (toList)
import Data.Ratio (denominator, numerator, (%))
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty (NESeq (..))
import Data.Sequence.NonEmpty qualified as NESeq
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Minipat.Print (Brace (..), Sep (..), braceCloseChar, braceOpenChar, sepChar)
import Prettyprinter (Doc, Pretty (..))
import Prettyprinter qualified as P

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
measTotal (Measure reps width) = (reps % 1) * width

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

-- * Ident

-- | A general textual identifier
newtype Ident = Ident {unIdent :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Pretty)

-- * QuickRatio

-- | Ratios represented by letters - see 'quickRatioValue' and 'quickRatioRep'
data QuickRatio
  = QuickRatioWhole
  | QuickRatioHalf
  | QuickRatioQuarter
  | QuickRatioEighth
  | QuickRatioSixteenth
  | QuickRatioThird
  | QuickRatioFifth
  | QuickRatioSixth
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Pretty QuickRatio where
  pretty a = pretty (quickRatioRep a)

-- | Fraction of cycle length represented by the 'QuickRatio'
quickRatioValue :: QuickRatio -> Rational
quickRatioValue = \case
  QuickRatioWhole -> 1
  QuickRatioHalf -> 1 % 2
  QuickRatioQuarter -> 1 % 4
  QuickRatioEighth -> 1 % 8
  QuickRatioSixteenth -> 1 % 16
  QuickRatioThird -> 1 % 3
  QuickRatioFifth -> 1 % 5
  QuickRatioSixth -> 1 % 6

-- | Renders a 'QuickRatio' as a character
quickRatioRep :: QuickRatio -> Char
quickRatioRep = \case
  QuickRatioWhole -> 'w'
  QuickRatioHalf -> 'h'
  QuickRatioQuarter -> 'q'
  QuickRatioEighth -> 'e'
  QuickRatioSixteenth -> 's'
  QuickRatioThird -> 't'
  QuickRatioFifth -> 'f'
  QuickRatioSixth -> 'x'

-- | Parses a 'QuickRatio' from a character
quickRatioUnRep :: Char -> Maybe QuickRatio
quickRatioUnRep = \case
  'w' -> Just QuickRatioWhole
  'h' -> Just QuickRatioHalf
  'q' -> Just QuickRatioQuarter
  'e' -> Just QuickRatioEighth
  's' -> Just QuickRatioSixteenth
  't' -> Just QuickRatioThird
  'f' -> Just QuickRatioFifth
  'x' -> Just QuickRatioSixth
  _ -> Nothing

-- * Factor

-- | Presentation of a numeric 'Rational' - decimal or fraction.
-- (This is for round-tripping parse to print.)
data RationalPres = RationalPresDec | RationalPresFrac
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A 'Rational' with various presentations: as decimal, fraction, integer, or character.
data Factor
  = FactorRational !RationalPres !Rational
  | FactorInteger !Integer
  | FactorQuickRatio !QuickRatio
  deriving stock (Eq, Ord, Show)

instance Pretty Factor where
  pretty = \case
    FactorRational rp rat ->
      case rp of
        RationalPresDec -> pretty @Double (fromRational rat)
        RationalPresFrac -> P.hcat ["(", pretty (numerator rat), "/", pretty (denominator rat), ")"]
    FactorInteger i -> pretty i
    FactorQuickRatio qr -> pretty qr

-- | The 'Rational' represented by this 'Factor'
factorValue :: Factor -> Rational
factorValue = \case
  FactorRational _ r -> r
  FactorInteger i -> fromInteger i
  FactorQuickRatio qr -> quickRatioValue qr

-- | An easy way to add 1 to a 'Factor'
factorSucc :: Factor -> Factor
factorSucc = \case
  FactorRational p r -> FactorRational p (r + 1)
  FactorInteger i -> FactorInteger (succ i)
  FactorQuickRatio qr -> FactorRational RationalPresFrac (quickRatioValue qr + 1)

-- * Mod

-- | An expression modified by some control.
data Mod c r = Mod
  { modTarget :: !r
  , modValue :: !c
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty c, Pretty r) => Pretty (Mod c r) where
  pretty (Mod tar val) = P.hcat [pretty tar, pretty val]

-- * Speed

-- | Speedup or slowdown
data SpeedDir = SpeedDirFast | SpeedDirSlow
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Pretty SpeedDir where
  pretty = \case
    SpeedDirFast -> "*"
    SpeedDirSlow -> "/"

-- | Speed control
data Speed s = Speed
  { speedDir :: !SpeedDir
  , speedFactor :: !s
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty s) => Pretty (Speed s) where
  pretty (Speed dir facs) = P.hcat [pretty dir, pretty facs]

-- * Select

-- | Select control
data Select = SelectSample !Integer | SelectTransform !Ident
  deriving stock (Eq, Ord, Show)

instance Pretty Select where
  pretty s =
    ":" <> case s of
      SelectSample i -> pretty i
      SelectTransform t -> pretty t

-- * Timing

-- | Shorthand for time control
data ShortTime = ShortTimeElongate | ShortTimeReplicate
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Pretty ShortTime where
  pretty = \case
    ShortTimeElongate -> "_"
    ShortTimeReplicate -> "!"

-- | Longhand for time control
data LongTime
  = LongTimeElongate !Factor
  | LongTimeReplicate !(Maybe Integer)
  deriving stock (Eq, Ord, Show)

instance Pretty LongTime where
  pretty = \case
    LongTimeElongate f -> P.hcat ["@", pretty f]
    LongTimeReplicate mi -> "!" <> maybe mempty pretty mi

-- | Time control that can be applied to an expression.
data Time r
  = TimeShort !ShortTime
  | TimeLong !r !LongTime
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty r) => Pretty (Time r) where
  pretty = \case
    TimeShort s -> pretty s
    TimeLong r l -> pretty r <> pretty l

-- * Generic group presentation

-- | Presentation of a sequence - dot- or space-separated
data SeqPres = SeqPresDot | SeqPresSpace
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Separator character for a given sequence presentation
seqPresSep :: SeqPres -> Maybe Sep
seqPresSep = \case
  SeqPresDot -> Just SepDot
  SeqPresSpace -> Nothing

-- | Renders a sequence of elements with the given punctuation and nesting.
prettyGroup :: Maybe Brace -> Maybe Sep -> Int -> [Doc ann] -> Doc ann
prettyGroup mb ms lvl ds =
  let prefix = pretty (replicate lvl '[') <> maybe mempty (pretty . braceOpenChar) mb
      suffix = maybe mempty (pretty . braceCloseChar) mb <> pretty (replicate lvl ']')
      body = P.hsep (maybe ds (\s -> P.punctuate (" " <> pretty (sepChar s)) ds) ms)
  in  prefix <> body <> suffix

-- * Patterns

-- ** Groups

data GroupPatType
  = GroupPatTypeSeq !SeqPres
  | GroupPatTypePar
  | GroupPatTypeRand
  | GroupPatTypeAlt
  deriving stock (Eq, Ord, Show)

groupPatTypeBrace :: GroupPatType -> Maybe Brace
groupPatTypeBrace = \case
  GroupPatTypeSeq _ -> Nothing
  GroupPatTypePar -> Nothing
  GroupPatTypeRand -> Nothing
  GroupPatTypeAlt -> Just BraceAngle

groupPatTypeSep :: GroupPatType -> Maybe Sep
groupPatTypeSep = \case
  GroupPatTypeSeq sp -> seqPresSep sp
  GroupPatTypePar -> Just SepComma
  GroupPatTypeRand -> Just SepPipe
  GroupPatTypeAlt -> Nothing

data GroupPat r = GroupPat
  { gpLevel :: !Int
  , gpType :: !GroupPatType
  , gpElems :: !(NESeq r)
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty r) => Pretty (GroupPat r) where
  pretty (GroupPat lvl ty ps) =
    let mb = groupPatTypeBrace ty
        ms = groupPatTypeSep ty
        ds = fmap pretty (toList ps)
    in  prettyGroup mb ms lvl ds

-- ** Mods

data Euclid = Euclid !Integer !Integer !(Maybe Integer)
  deriving stock (Eq, Ord, Show)

instance Pretty Euclid where
  pretty (Euclid i j mk) =
    P.hcat $
      ["(", pretty i, ",", pretty j] ++ maybe [")"] (\k -> [",", pretty k, ")"]) mk

newtype Degrade = Degrade {unDegrade :: Maybe Factor}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Pretty Degrade where
  pretty (Degrade mfp) = P.hcat ["?", maybe mempty pretty mfp]

data ModPat s
  = ModPatSelect !Select
  | ModPatDegrade !Degrade
  | ModPatEuclid !Euclid
  | ModPatSpeed !(Speed s)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty s) => Pretty (ModPat s) where
  pretty = \case
    ModPatDegrade d -> pretty d
    ModPatEuclid e -> pretty e
    ModPatSelect s -> pretty s
    ModPatSpeed s -> pretty s

-- ** Polymeters

data PolyPat r = PolyPat
  { ppElems :: !(NESeq r)
  , ppSteps :: !(Maybe Integer)
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty r) => Pretty (PolyPat r) where
  pretty (PolyPat ps mc) =
    let ds = fmap pretty (toList ps)
        body = P.hsep (P.punctuate " ," ds)
        trailer = maybe mempty (\i -> "%" <> pretty i) mc
    in  "{" <> body <> "}" <> trailer

-- ** Functor

-- TODO poly/euclid can take sig args?
data PatF s a r
  = PatPure !a
  | PatSilence
  | PatTime !(Time r)
  | PatGroup !(GroupPat r)
  | PatMod !(Mod (ModPat s) r)
  | PatPoly !(PolyPat r)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor (PatF s) where
  bimap f g = \case
    PatPure a -> PatPure (f a)
    PatSilence -> PatSilence
    PatTime t -> PatTime (fmap g t)
    PatGroup gs -> PatGroup (fmap g gs)
    PatMod m -> PatMod (fmap g m)
    PatPoly p -> PatPoly (fmap g p)

instance Bifoldable (PatF s) where
  bifoldr f g = go
   where
    go z = \case
      PatPure a -> f a z
      PatSilence -> z
      PatTime t -> foldr g z t
      PatGroup gs -> foldr g z gs
      PatMod m -> foldr g z m
      PatPoly p -> foldr g z p

instance Bitraversable (PatF s) where
  bitraverse f g = \case
    PatPure a -> fmap PatPure (f a)
    PatSilence -> pure PatSilence
    PatTime t -> fmap PatTime (traverse g t)
    PatGroup gs -> fmap PatGroup (traverse g gs)
    PatMod m -> fmap PatMod (traverse g m)
    PatPoly p -> fmap PatPoly (traverse g p)

instance (IsString a) => IsString (PatF s a r) where
  fromString = PatPure . fromString

instance (Pretty s, Pretty a, Pretty r) => Pretty (PatF s a r) where
  pretty a = case a of
    PatPure x -> pretty x
    PatSilence -> "~"
    PatTime t -> pretty t
    PatGroup gp -> pretty gp
    PatMod m -> pretty m
    PatPoly p -> pretty p

-- ** Fixpoint

newtype Pat b a = Pat {unPat :: UnPat b a}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Functor, Foldable, Pretty)

type UnPat b = Jot (PatF (Pat b Factor)) b

type PatK b a = PatF (Pat b Factor) a (UnPat b a)

instance Traversable (Pat a) where traverse f = fmap Pat . traverse f . unPat

instance Bifunctor Pat where
  bimap f g = Pat . go . unPat
   where
    go (JotP b pf) = JotP (f b) $
      case pf of
        PatPure a -> PatPure (g a)
        PatSilence -> PatSilence
        PatTime t -> PatTime (fmap go t)
        PatGroup gs -> PatGroup (fmap go gs)
        PatMod (Mod r m) -> PatMod (Mod (go r) (fmap (first f) m))
        PatPoly (PolyPat rs mc) -> PatPoly (PolyPat (fmap go rs) mc)

instance Bifoldable Pat where
  bifoldr f g = flip (go . unPat)
   where
    go (JotP b pf) z = f b $
      case pf of
        PatPure a -> g a z
        PatSilence -> z
        PatTime t -> foldr go z t
        PatGroup gs -> foldr go z gs
        PatMod (Mod r m) -> go r (foldr (flip (bifoldr f (const id))) z m)
        PatPoly (PolyPat rs _) -> foldr go z rs

instance Bitraversable Pat where
  bitraverse f g = fmap Pat . go . unPat
   where
    go (JotP b pf) =
      JotP
        <$> f b
        <*> case pf of
          PatPure a -> fmap PatPure (g a)
          PatSilence -> pure PatSilence
          PatTime t -> fmap PatTime (traverse go t)
          PatGroup gs -> fmap PatGroup (traverse go gs)
          PatMod (Mod r m) -> fmap PatMod $ Mod <$> go r <*> traverse (bitraverse f pure) m
          PatPoly (PolyPat rs mc) -> fmap (\rs' -> PatPoly (PolyPat rs' mc)) (traverse go rs)

-- ** Normalization

type NPat b = Pat (Expansion b)

type UnNPat b = UnPat (Expansion b)

type NPatK b a = PatK (Expansion b) a

wrapPatM :: NPatK b a -> NormM b (NPat b a)
wrapPatM ff = asks (\(b :<|| _) -> Pat (JotP (expNew b) ff))

rewrapPatM :: (Semigroup b) => UnPat b a -> (UnNPat b a -> NormM b (NPatK b a)) -> NormM b (NPat b a)
rewrapPatM r f = do
  p <- normPatM (Pat r)
  let Pat r'@(JotP ex _) = p
  k <- f r'
  asks (\(b :<|| _) -> Pat (JotP (ex {expInfo = b}) k))

normModPatM :: (Semigroup b) => ModPat (Pat b a) -> NormM b (ModPat (NPat b a))
normModPatM = \case
  ModPatSelect s -> pure (ModPatSelect s)
  ModPatSpeed s -> fmap ModPatSpeed (traverse normPatM s)
  ModPatDegrade d -> pure (ModPatDegrade d)
  ModPatEuclid e -> pure (ModPatEuclid e)

foldNormPatKM :: (Semigroup b) => Int -> GroupPatType -> NESeq (UnPat b a) -> NormM b (NPat b a)
foldNormPatKM lvl ty = goFirst
 where
  goFirst (y :<|| ys) = do
    Pat w <- normPatM (Pat y)
    goRest (NESeq.singleton w) ys
  goRest ws@(wsi :||> JotP (Expansion (Measure xi yi) vi) ffi) = \case
    Empty -> wrapPatM (PatGroup (GroupPat lvl ty ws))
    JotP b ff :<| ys -> do
      ws' <- case ff of
        PatTime (TimeShort s) -> do
          let m = case s of
                ShortTimeElongate -> Measure xi (yi + 1)
                ShortTimeReplicate -> Measure (xi + 1) yi
              v = vi <> b
              w = JotP (Expansion m v) ffi
          pure (wsi :||> w)
        _ -> do
          Pat w <- local (b NESeq.<|) (normPatKM ff)
          pure (ws NESeq.|> w)
      goRest ws' ys

normPatKM :: (Semigroup b) => PatK b a -> NormM b (NPat b a)
normPatKM = \case
  PatPure a -> wrapPatM (PatPure a)
  PatSilence -> wrapPatM PatSilence
  PatTime t ->
    case t of
      -- Time shorthands at top level are nonsense - throw error
      TimeShort _ -> ask >>= throwError . NormErr
      -- Annotated time expressions turn into decorations
      TimeLong r l -> do
        p <- normPatM (Pat r)
        let Pat (JotP (Expansion (Measure xi yi) vi) ff') = p
        let m = case l of
              LongTimeElongate f -> Measure xi (yi * factorValue f)
              LongTimeReplicate mi -> Measure (maybe (xi + 1) (xi *) mi) yi
        asks $ \(b :<|| _) ->
          let v = vi <> b
          in  Pat (JotP (Expansion m v) ff')
  PatGroup (GroupPat lvl ty ss) ->
    case ss of
      -- Unwrap any singletons we find
      q :<|| Empty -> normPatM (Pat q)
      -- Otherwise normalize by folding
      _ -> foldNormPatKM lvl ty ss
  PatMod (Mod r m) ->
    -- Just propagate time controls upward
    rewrapPatM r $ \r' -> do
      m' <- normModPatM m
      pure (PatMod (Mod r' m'))
  PatPoly (PolyPat rs mc) -> do
    -- Just recurse and reset time controls here
    rs' <- traverse (fmap unPat . normPatM . Pat) rs
    wrapPatM (PatPoly (PolyPat rs' mc))

normPatM :: (Semigroup b) => Pat b a -> NormM b (NPat b a)
normPatM (Pat (JotP b ff)) = local (b NESeq.<|) (normPatKM ff)

normPat :: (Semigroup b) => Pat b a -> Either (NormErr b) (NPat b a)
normPat (Pat (JotP b ff)) = runNormM (normPatKM ff) b
