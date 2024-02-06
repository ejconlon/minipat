{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Attrs
  ( DatumProxy (..)
  , datumProxyType
  , Attr (..)
  , Attrs
  , attrs
  , IsAttrs (..)
  )
where

import Dahdit.Midi.Osc (Datum (..), DatumType (..), IsDatum (..))
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Minipat.Ast (Ident (..))
import Minipat.Dirt.Notes (Note (..))

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

data Attr a = Attr
  { attrKey :: !Text
  , attrVal :: !a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type Attrs = Map Text Datum

attrs :: [(Text, Datum)] -> Attrs
attrs = Map.fromList

class IsAttrs a where
  toAttrs :: a -> Attrs

instance IsAttrs Attrs where
  toAttrs = id

instance (IsDatum a) => IsAttrs (Attr a) where
  toAttrs (Attr k v) = Map.singleton k (toDatum v)

instance IsAttrs Note where
  toAttrs (Note n) = Map.singleton "note" (DatumInt32 (fromInteger n))

instance IsAttrs Sound where
  toAttrs (Sound s mn) = Map.insert "sound" (DatumString (unIdent s)) (maybe Map.empty toAttrs mn)
