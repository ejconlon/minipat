{-# LANGUAGE OverloadedStrings #-}

module Minipat.Live.Datum
  ( DatumProxy (..)
  , datumProxyType
  , prettyDatum
  )
where

import Dahdit.Midi.Osc (Datum (..), DatumType (..))
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
