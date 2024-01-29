{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Prelude where

import Dahdit.Midi.Osc (DatumType (..), IsDatum (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Minipat.Base qualified as B
import Minipat.Dirt.Eval (liveEvalPat)
import Minipat.Dirt.Osc (OscMap)

setIn, (#) :: B.Pat OscMap -> B.Pat OscMap -> B.Pat OscMap
setIn p1 p2 = B.patInnerBind p1 (\m1 -> fmap (m1 <>) p2)
(#) = setIn

ppat :: DatumType -> Text -> Text -> B.Pat OscMap
ppat dt k t = pat k (liveEvalPat dt t)

pat :: (IsDatum a) => Text -> B.Pat a -> B.Pat OscMap
pat k = fmap (Map.singleton k . toDatum)

sound, s :: Text -> B.Pat OscMap
sound = ppat DatumTypeString "sound"
s = sound

note, n :: Text -> B.Pat OscMap
note = ppat DatumTypeInt32 "note"
n = note
