module Minipat.Dirt.Eval where

import Control.Exception (throw)
import Dahdit.Midi.Osc (Datum (..), DatumType (..))
import Data.Text (Text)
import Looksee as L
import Minipat.Ast qualified as A
import Minipat.Base qualified as B
import Minipat.Eval qualified as E
import Minipat.Interp qualified as I
import Minipat.Parser qualified as P

datumP :: DatumType -> P.P Datum
datumP = \case
  DatumTypeInt32 -> fmap (DatumInt32 . fromInteger) L.intP
  DatumTypeInt64 -> fmap (DatumInt64 . fromInteger) L.intP
  DatumTypeFloat -> fmap (DatumFloat . realToFrac) L.sciP
  DatumTypeDouble -> fmap (DatumDouble . realToFrac) L.sciP
  DatumTypeString -> fmap (DatumString . A.unIdent) P.identP
  dt -> fail ("Datum type is not parseable: " <> show dt)

liveEvalPat :: DatumType -> Text -> B.Pat Datum
liveEvalPat dt t = either throw id (E.evalPat I.noSelFn I.noSelFn (datumP dt) t)
