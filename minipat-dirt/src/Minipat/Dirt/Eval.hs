module Minipat.Dirt.Eval where

import Control.Exception (throw)
import Dahdit.Midi.Osc (Datum (..), DatumType (..))
import Data.Text (Text)
import Looksee as L
import Minipat.Ast (Ident (..))
import Minipat.Eval (evalPat)
import Minipat.Interp (noSelFn)
import Minipat.Parser (P, identP)
import Minipat.Stream (Stream)

datumP :: DatumType -> P Datum
datumP = \case
  DatumTypeInt32 -> fmap (DatumInt32 . fromInteger) L.intP
  DatumTypeInt64 -> fmap (DatumInt64 . fromInteger) L.intP
  DatumTypeFloat -> fmap (DatumFloat . realToFrac) L.sciP
  DatumTypeDouble -> fmap (DatumDouble . realToFrac) L.sciP
  DatumTypeString -> fmap (DatumString . unIdent) identP
  dt -> fail ("Datum type is not parseable: " <> show dt)

liveEvalPat :: DatumType -> Text -> Stream Datum
liveEvalPat dt t = either throw id (evalPat noSelFn noSelFn (datumP dt) t)
