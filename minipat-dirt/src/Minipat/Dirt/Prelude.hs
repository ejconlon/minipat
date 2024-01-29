{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Prelude where

import Dahdit.Midi.Osc (DatumType (..), IsDatum (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Minipat.Dirt.Eval (liveEvalPat)
import Minipat.Dirt.Osc (OscMap)
import Minipat.Stream (Stream (..), streamInnerBind)

setIn, (#) :: Stream OscMap -> Stream OscMap -> Stream OscMap
setIn p1 p2 = streamInnerBind p1 (\m1 -> fmap (m1 <>) p2)
(#) = setIn

pat :: DatumType -> Text -> Text -> Stream OscMap
pat dt k t = stream k (liveEvalPat dt t)

stream :: (IsDatum a) => Text -> Stream a -> Stream OscMap
stream k = fmap (Map.singleton k . toDatum)

sound, s :: Text -> Stream OscMap
sound = pat DatumTypeString "sound"
s = sound

note, n :: Text -> Stream OscMap
note = pat DatumTypeInt32 "note"
n = note
