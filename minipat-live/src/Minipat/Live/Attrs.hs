{-# LANGUAGE OverloadedStrings #-}

module Minipat.Live.Attrs
  ( DatumProxy (..)
  , datumProxyType
  , Attr (..)
  , Attrs
  , attrsSingleton
  , attrsFromList
  , attrsLookup
  , attrsInsert
  , attrsDefault
  , attrsDelete
  , attrsToList
  , DupeAttrErr
  , attrsTryInsert
  , attrsUnalias
  , Squishy (..)
  , squishMerge
  )
where

import Control.Exception (Exception)
import Control.Monad (foldM)
import Dahdit.Midi.Osc (Datum (..), DatumType (..), IsDatum (..))
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Minipat.Ast (Ident (..))
import Minipat.Live.Notes (Note (..))
import Prettyprinter (Doc, Pretty (..))
import Prettyprinter qualified as P

data Sound = Sound
  { soundIdent :: !Ident
  , soundNote :: !(Maybe Note)
  }
  deriving stock (Eq, Ord, Show)

data DatumProxy a where
  DatumProxyInt32 :: DatumProxy Int32
  DatumProxyInt64 :: DatumProxy Int64
  DatumProxyFloat :: DatumProxy Float
  DatumProxyDouble :: DatumProxy Double
  DatumProxyString :: DatumProxy Text

datumProxyType :: DatumProxy a -> DatumType
datumProxyType = \case
  DatumProxyInt32 -> DatumTypeInt32
  DatumProxyInt64 -> DatumTypeInt64
  DatumProxyFloat -> DatumTypeFloat
  DatumProxyDouble -> DatumTypeDouble
  DatumProxyString -> DatumTypeString

prettyDatum :: Datum -> Doc ann
prettyDatum = \case
  DatumInt32 x -> pretty x
  DatumInt64 x -> pretty x
  DatumFloat x -> pretty x
  DatumDouble x -> pretty x
  DatumString x -> pretty x
  DatumBlob _ -> "<BLOB>"
  DatumTime _ -> "<TIME>"
  DatumMidi _ -> "<MIDI>"

data Attr a = Attr
  { attrKey :: !Text
  , attrVal :: !a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Attrs = Attrs {unAttrs :: Map Text Datum}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Semigroup Attrs where
  -- Flip order to allow keys on the right to win
  Attrs m1 <> Attrs m2 = Attrs (m2 <> m1)

instance Monoid Attrs where
  mempty = Attrs Map.empty

instance Pretty Attrs where
  pretty as = P.hsep ("{" : P.punctuate "," (fmap (\(k, v) -> P.hcat [pretty k, ": ", prettyDatum v]) (attrsToList as)) ++ ["}"])

attrsSingleton :: Text -> Datum -> Attrs
attrsSingleton k v = Attrs (Map.singleton k v)

attrsFromList :: [(Text, Datum)] -> Attrs
attrsFromList = Attrs . Map.fromList

attrsLookup :: Text -> Attrs -> Maybe Datum
attrsLookup k (Attrs m) = Map.lookup k m

attrsInsert :: Text -> Datum -> Attrs -> Attrs
attrsInsert k v (Attrs m) = Attrs (Map.insert k v m)

attrsDefault :: Text -> Datum -> Attrs -> Attrs
attrsDefault k v a@(Attrs m) = case Map.lookup k m of
  Nothing -> Attrs (Map.insert k v m)
  Just _ -> a

attrsDelete :: Text -> Attrs -> Attrs
attrsDelete k (Attrs m) = Attrs (Map.delete k m)

attrsToList :: Attrs -> [(Text, Datum)]
attrsToList = Map.toList . unAttrs

newtype DupeAttrErr = DupeAttrErr {unDupeAttrErr :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Exception DupeAttrErr

attrsTryInsert :: Text -> Datum -> Attrs -> Either DupeAttrErr Attrs
attrsTryInsert k v m =
  case attrsLookup k m of
    Nothing -> Right (attrsInsert k v m)
    Just _ -> Left (DupeAttrErr k)

attrsUnalias :: [(Text, Text)] -> Attrs -> Either DupeAttrErr Attrs
attrsUnalias as m0 = foldM go m0 as
 where
  go !m (x, y) = do
    case attrsLookup x m of
      Nothing -> pure m
      Just v -> attrsTryInsert y v (attrsDelete x m)

class (Semigroup q) => Squishy q a where
  squish :: a -> q

squishMerge :: (Squishy q a, Squishy q b) => a -> b -> q
squishMerge a b = squish a <> squish b

instance {-# OVERLAPPABLE #-} (Semigroup q) => Squishy q q where
  squish = id

instance {-# INCOHERENT #-} (Monoid q, Squishy q a) => Squishy q (Maybe a) where
  squish = maybe mempty squish

instance (IsDatum a) => Squishy Attrs (Attr a) where
  squish (Attr k v) = attrsSingleton k (toDatum v)

instance Squishy Attrs Note where
  squish (Note n) = attrsSingleton "note" (DatumInt32 (fromInteger n))
