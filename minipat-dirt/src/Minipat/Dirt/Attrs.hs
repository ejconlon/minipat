{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Attrs
  ( DatumProxy (..)
  , datumProxyType
  , Attr (..)
  , Attrs
  , attrsSingleton
  , attrsFromList
  , attrsLookup
  , attrsInsert
  , attrsDelete
  , attrsToList
  , IsAttrs (..)
  , attrsMerge
  )
where

import Dahdit.Midi.Osc (Datum (..), DatumType (..), IsDatum (..))
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Minipat.Ast (Ident (..))
import Minipat.Dirt.Notes (Note (..))
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

attrsDelete :: Text -> Attrs -> Attrs
attrsDelete k (Attrs m) = Attrs (Map.delete k m)

attrsToList :: Attrs -> [(Text, Datum)]
attrsToList = Map.toList . unAttrs

class IsAttrs a where
  toAttrs :: a -> Attrs

instance IsAttrs Attrs where
  toAttrs = id

instance (IsDatum a) => IsAttrs (Attr a) where
  toAttrs (Attr k v) = attrsSingleton k (toDatum v)

instance IsAttrs Note where
  toAttrs (Note n) = attrsSingleton "note" (DatumInt32 (fromInteger n))

attrsMerge :: (IsAttrs a, IsAttrs b) => a -> b -> Attrs
attrsMerge a b = toAttrs a <> toAttrs b
