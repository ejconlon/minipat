{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | AST used when parsing/printing Tidal mini-notation
module Minipat.Ast
  ( Ident (..)
  , Select (..)
  , QuickRatio (..)
  , quickRatioValue
  , quickRatioRep
  , quickRatioUnRep
  , RationalPres (..)
  , Factor (..)
  , factorFromRational
  , factorValue
  , factorSucc
  , Short (..)
  , SpeedDir (..)
  , Speed (..)
  , Euclid (..)
  , Degrade (..)
  , ModType (..)
  , Mod (..)
  , Elongate (..)
  , Replicate (..)
  , SeqPres (..)
  , GroupType (..)
  , Group (..)
  , Poly (..)
  , PatF (..)
  , Pat (..)
  , UnPat
  , Pattern (..)
  )
where

import Bowtie (Jot (..), pattern JotP)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Foldable (toList)
import Data.Ratio (denominator, numerator, (%))
import Data.Sequence (Seq (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Minipat.Print (Brace (..), Sep (..), braceCloseChar, braceOpenChar, sepChar)
import Prettyprinter (Doc, Pretty (..))
import Prettyprinter qualified as P

-- * Ident

-- | A general textual identifier
newtype Ident = Ident {unIdent :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Pretty)

-- * Select

-- | A selection of some kind - note, transformation, etc.
data Select s a = Select !a !(Maybe s)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty s, Pretty a) => Pretty (Select s a) where
  pretty (Select a ms) =
    case ms of
      Nothing -> pretty a
      Just s -> P.hcat [pretty a, ":", pretty s]

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

-- | Make a 'Factor' representing the given 'Rational'
factorFromRational :: Rational -> Factor
factorFromRational = FactorRational RationalPresDec

factorUnary :: (forall a. (Num a) => a -> a) -> Factor -> Factor
factorUnary f = \case
  FactorRational rp rat -> FactorRational rp (f rat)
  FactorInteger i -> FactorInteger (f i)
  FactorQuickRatio qr -> FactorRational RationalPresDec (f (quickRatioValue qr))

factorBinary :: (forall a. (Num a) => a -> a -> a) -> Factor -> Factor -> Factor
factorBinary f (FactorInteger i) (FactorInteger j) = FactorInteger (f i j)
factorBinary f x y = factorFromRational (f (factorValue x) (factorValue y))

instance Num Factor where
  (+) = factorBinary (+)
  (-) = factorBinary (-)
  (*) = factorBinary (*)
  abs = factorUnary abs
  signum = factorUnary signum
  fromInteger = FactorInteger
  negate = factorUnary negate

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

-- * Shorthand

-- | Shorthand for elongate/replicate - attaches to previous element in sequence
data Short = ShortElongate | ShortReplicate
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Pretty Short where
  pretty = \case
    ShortElongate -> "_"
    ShortReplicate -> "!"

-- * Mod

-- | Speedup or slowdown
data SpeedDir = SpeedDirFast | SpeedDirSlow
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Pretty SpeedDir where
  pretty = \case
    SpeedDirFast -> "*"
    SpeedDirSlow -> "/"

-- | Speed control
data Speed b = Speed
  { speedDir :: !SpeedDir
  , speedFactor :: !(Pat b Factor)
  }
  deriving stock (Eq, Ord, Show)

instance Pretty (Speed b) where
  pretty (Speed dir facs) = P.hcat [pretty dir, pretty facs]

-- | Euclidean sequences
data Euclid = Euclid
  { eucFilled :: !Integer
  , eucSteps :: !Integer
  , eucShift :: !(Maybe Integer)
  }
  deriving stock (Eq, Ord, Show)

instance Pretty Euclid where
  pretty (Euclid i j mk) =
    P.hcat $
      ["(", pretty i, ",", pretty j] ++ maybe [")"] (\k -> [",", pretty k, ")"]) mk

-- | Degradation (random dropout)
newtype Degrade b = Degrade {unDegrade :: Maybe (Pat b Factor)}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Pretty (Degrade b) where
  pretty (Degrade mfp) = P.hcat ["?", maybe mempty pretty mfp]

-- | Elongate element by the given factor
newtype Elongate = Elongate {unElongate :: Factor}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Pretty Elongate where
  pretty (Elongate f) = P.hcat ["@", pretty f]

-- | Replicate element by the given factor
newtype Replicate = Replicate {unReplicate :: Maybe Integer}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Pretty Replicate where
  pretty (Replicate mi) = "!" <> maybe mempty pretty mi

-- TODO add elongate/replicate constructors here

-- | Controls that can be applied to a given pattern
data ModType b
  = ModTypeDegrade !(Degrade b)
  | ModTypeEuclid !Euclid
  | ModTypeSpeed !(Speed b)
  | ModTypeElongate !Elongate
  | ModTypeReplicate !Replicate
  deriving stock (Eq, Ord, Show)

instance Pretty (ModType b) where
  pretty = \case
    ModTypeDegrade x -> pretty x
    ModTypeEuclid x -> pretty x
    ModTypeSpeed x -> pretty x
    ModTypeElongate x -> pretty x
    ModTypeReplicate x -> pretty x

-- | An expression modified by some control
data Mod b r = Mod
  { modTarget :: !r
  , modType :: !(ModType b)
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty r) => Pretty (Mod b r) where
  pretty (Mod tar ty) = P.hcat [pretty tar, pretty ty]

-- * Groups

-- | Presentation of a sequence - dot- or space-separated
data SeqPres = SeqPresDot | SeqPresSpace
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Separator character for a given sequence presentation
seqPresSep :: SeqPres -> Maybe Sep
seqPresSep = \case
  SeqPresDot -> Just SepDot
  SeqPresSpace -> Nothing

-- | Renders a sequence of elements with the given punctuation and nesting
prettyGroup :: Maybe Brace -> Maybe Sep -> Integer -> [Doc ann] -> Doc ann
prettyGroup mb ms (fromInteger -> lvl) ds =
  let prefix = pretty (replicate lvl '[') <> maybe mempty (pretty . braceOpenChar) mb
      suffix = maybe mempty (pretty . braceCloseChar) mb <> pretty (replicate lvl ']')
      body = P.hsep (maybe ds (\s -> P.punctuate (" " <> pretty (sepChar s)) ds) ms)
  in  prefix <> body <> suffix

-- | The type of group - sequential, parallel, random, or alternating
data GroupType
  = GroupTypeSeq !SeqPres
  | GroupTypePar
  | GroupTypeRand
  | GroupTypeAlt
  deriving stock (Eq, Ord, Show)

groupPatTypeBrace :: GroupType -> Maybe Brace
groupPatTypeBrace = \case
  GroupTypeSeq _ -> Nothing
  GroupTypePar -> Nothing
  GroupTypeRand -> Nothing
  GroupTypeAlt -> Just BraceAngle

groupPatTypeSep :: GroupType -> Maybe Sep
groupPatTypeSep = \case
  GroupTypeSeq sp -> seqPresSep sp
  GroupTypePar -> Just SepComma
  GroupTypeRand -> Just SepPipe
  GroupTypeAlt -> Nothing

-- | A group of sub-patterns
data Group r = Group
  { groupLevel :: !Integer
  , groupType :: !GroupType
  , groupElems :: !(Seq r)
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty r) => Pretty (Group r) where
  pretty (Group lvl ty ps) =
    let mb = groupPatTypeBrace ty
        ms = groupPatTypeSep ty
        ds = fmap pretty (toList ps)
    in  prettyGroup mb ms lvl ds

-- * Polymeters

-- | A polymeter wrapping at the given number of steps
data Poly r = Poly
  { polyElems :: !(Seq r)
  , polySteps :: !(Maybe Integer)
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty r) => Pretty (Poly r) where
  pretty (Poly ps mc) =
    let ds = fmap pretty (toList ps)
        body = P.hsep (P.punctuate " ," ds)
        trailer = maybe mempty (\i -> "%" <> pretty i) mc
    in  "{" <> body <> "}" <> trailer

-- * Functor

data PatF b a r
  = PatPure !a
  | PatSilence
  | PatShort !Short
  | PatGroup !(Group r)
  | PatMod !(Mod b r)
  | PatPoly !(Poly r)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor (PatF b) where
  bimap f g = \case
    PatPure a -> PatPure (f a)
    PatSilence -> PatSilence
    PatShort s -> PatShort s
    PatGroup gs -> PatGroup (fmap g gs)
    PatMod m -> PatMod (fmap g m)
    PatPoly p -> PatPoly (fmap g p)

instance Bifoldable (PatF b) where
  bifoldr f g = go
   where
    go z = \case
      PatPure a -> f a z
      PatSilence -> z
      PatShort _ -> z
      PatGroup gs -> foldr g z gs
      PatMod m -> foldr g z m
      PatPoly p -> foldr g z p

instance Bitraversable (PatF b) where
  bitraverse f g = \case
    PatPure a -> fmap PatPure (f a)
    PatSilence -> pure PatSilence
    PatShort s -> pure (PatShort s)
    PatGroup gs -> fmap PatGroup (traverse g gs)
    PatMod m -> fmap PatMod (traverse g m)
    PatPoly p -> fmap PatPoly (traverse g p)

instance (IsString a) => IsString (PatF b a r) where
  fromString = PatPure . fromString

instance (Pretty a, Pretty r) => Pretty (PatF b a r) where
  pretty a = case a of
    PatPure x -> pretty x
    PatSilence -> "~"
    PatShort s -> pretty s
    PatGroup gp -> pretty gp
    PatMod m -> pretty m
    PatPoly p -> pretty p

-- * Fixpoint

-- | A pattern that can be printed or rendered into a 'Stream'
newtype Pat b a = Pat {unPat :: UnPat b a}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Functor, Foldable, Pretty)

type UnPat b = Jot (PatF b) b

instance Traversable (Pat a) where traverse f = fmap Pat . traverse f . unPat

instance Bifunctor Pat where
  bimap f g = goP
   where
    goP = Pat . goJ . unPat
    goJ (JotP b pf) = JotP (f b) (goG pf)
    goG = \case
      PatPure a -> PatPure (g a)
      PatSilence -> PatSilence
      PatShort s -> PatShort s
      PatGroup (Group lvl ty rs) -> PatGroup (Group lvl ty (fmap goJ rs))
      PatMod (Mod r m) -> PatMod (Mod (goJ r) (goM m))
      PatPoly (Poly rs mi) -> PatPoly (Poly (fmap goJ rs) mi)
    goM = \case
      ModTypeDegrade d -> ModTypeDegrade (goD d)
      ModTypeEuclid e -> ModTypeEuclid e
      ModTypeSpeed s -> ModTypeSpeed (goS s)
      ModTypeElongate e -> ModTypeElongate e
      ModTypeReplicate r -> ModTypeReplicate r
    goD (Degrade mp) = Degrade (fmap (first f) mp)
    goS (Speed d p) = Speed d (first f p)

instance Bifoldable Pat where
  bifoldr f g = goP
   where
    goP z = goJ z . unPat
    goJ z (JotP b pf) = f b (goG z pf)
    goG z = \case
      PatPure a -> g a z
      PatSilence -> z
      PatShort _ -> z
      PatGroup (Group _ _ rs) -> foldr (flip goJ) z rs
      PatMod (Mod r m) -> goJ (goM z m) r
      PatPoly (Poly rs _) -> foldr (flip goJ) z rs
    goM z = \case
      ModTypeDegrade d -> goD z d
      ModTypeEuclid _ -> z
      ModTypeSpeed s -> goS z s
      ModTypeElongate _ -> z
      ModTypeReplicate _ -> z
    goD z (Degrade mp) = maybe z (bifoldr f (\_ w -> w) z) mp
    goS z (Speed _ p) = bifoldr f (\_ w -> w) z p

instance Bitraversable Pat where
  bitraverse f g = goP
   where
    goP = fmap Pat . goJ . unPat
    goJ (JotP b pf) = liftA2 JotP (f b) (goG pf)
    goG = \case
      PatPure a -> fmap PatPure (g a)
      PatSilence -> pure PatSilence
      PatShort s -> pure (PatShort s)
      PatGroup (Group lvl ty rs) -> fmap (PatGroup . Group lvl ty) (traverse goJ rs)
      PatMod (Mod r m) -> liftA2 (\r' m' -> PatMod (Mod r' m')) (goJ r) (goM m)
      PatPoly (Poly rs mi) -> fmap (\rs' -> PatPoly (Poly rs' mi)) (traverse goJ rs)
    goM = \case
      ModTypeDegrade d -> fmap ModTypeDegrade (goD d)
      ModTypeEuclid e -> pure (ModTypeEuclid e)
      ModTypeSpeed s -> fmap ModTypeSpeed (goS s)
      ModTypeElongate e -> pure (ModTypeElongate e)
      ModTypeReplicate r -> pure (ModTypeReplicate r)
    goD (Degrade mp) = fmap Degrade (traverse (bitraverse f pure) mp)
    goS (Speed d p) = fmap (Speed d) (bitraverse f pure p)

mkPat :: (Monoid b) => PatF b a (UnPat b a) -> Pat b a
mkPat = Pat . JotP mempty

mkPatGroup :: (Monoid b) => GroupType -> Seq (Pat b a) -> Pat b a
mkPatGroup gt = \case
  Empty -> mkPat PatSilence
  x :<| Empty -> x
  xs -> mkPat (PatGroup (Group 0 gt (fmap unPat xs)))

mkPatSpeed :: (Monoid b) => SpeedDir -> Pat b Rational -> Pat b a -> Pat b a
mkPatSpeed sd pf (Pat pa) = mkPat (PatMod (Mod pa (ModTypeSpeed (Speed sd (fmap factorFromRational pf)))))

mkPatDeg :: (Monoid b) => Pat b Rational -> Pat b a -> Pat b a
mkPatDeg pf (Pat pa) = mkPat (PatMod (Mod pa (ModTypeDegrade (Degrade (Just (fmap factorFromRational pf))))))

-- mkPatSeq :: (Monoid b) => Seq (Pat b a, Rational) -> Pat b a
-- mkPatSeq = \case
--   Empty -> mkPat PatSilence
--   (x, _) :<| Empty -> x
--   xs -> mkPat (PatGroup (Group 0 (GroupTypeSeq SeqPresSpace) _

-- | 'Pat' and 'Stream' can be constructed abstractly with this
class (Functor f) => Pattern f where
  patPure :: a -> f a
  patEmpty :: f a
  patPar :: Seq (f a) -> f a
  patAlt :: Seq (f a) -> f a
  patRand :: Seq (f a) -> f a
  patSeq :: Seq (f a, Rational) -> f a
  patEuc :: Euclid -> f a -> f a
  patRep :: Integer -> f a -> f a
  patFast, patSlow :: f Rational -> f a -> f a
  patFastBy, patSlowBy :: Rational -> f a -> f a
  patDeg :: f Rational -> f a -> f a
  patDegBy :: Rational -> f a -> f a

instance (Monoid b) => Pattern (Pat b) where
  patPure = mkPat . PatPure
  patEmpty = mkPat PatSilence
  patPar = mkPatGroup GroupTypePar
  patAlt = mkPatGroup GroupTypeAlt
  patRand = mkPatGroup GroupTypeRand
  patSeq = error "TODO"
  patEuc = error "TODO"
  patRep = error "TODO"
  patFast = mkPatSpeed SpeedDirFast
  patSlow = mkPatSpeed SpeedDirSlow
  patFastBy = patFast . patPure
  patSlowBy = patSlow . patPure
  patDeg = mkPatDeg
  patDegBy = patDeg . patPure

-- TODO figure this out
--
-- ur
--   :: (Pattern f, Ord k)
--   => Pat (Select k Ident)
--   -> [(k, f a)]
--   -> [(Ident, f a -> f a)]
--   -> f a
-- ur p0 xs ys = go (Map.fromList xs) (Map.fromList ys) p0
