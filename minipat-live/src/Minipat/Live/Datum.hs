{-# LANGUAGE OverloadedStrings #-}

module Minipat.Live.Datum
  ( DatumProxy (..)
  , datumProxyType
  , prettyDatum
  , DatumTypeErr
  , castDatum
  )
where

import Control.Exception (Exception)
import Dahdit.Midi.Osc (Datum (..), DatumType (..), datumType)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Prettyprinter (Doc, Pretty (..))

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

data DatumTypeErr = DatumTypeErr
  { dteExpected :: !DatumType
  , dteActual :: !DatumType
  }
  deriving stock (Eq, Ord, Show)

instance Exception DatumTypeErr

castDatum :: DatumProxy a -> Datum -> Either DatumTypeErr a
castDatum DatumProxyInt32 (DatumInt32 x) = Right x
castDatum DatumProxyInt64 (DatumInt64 x) = Right x
castDatum DatumProxyFloat (DatumFloat x) = Right x
castDatum DatumProxyDouble (DatumDouble x) = Right x
castDatum DatumProxyString (DatumString x) = Right x
castDatum p d = Left (DatumTypeErr (datumProxyType p) (datumType d))
