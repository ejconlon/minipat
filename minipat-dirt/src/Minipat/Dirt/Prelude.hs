{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Prelude where

import Dahdit.Midi.Osc (DatumType (..), IsDatum (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Minipat.Dirt.Eval (liveEvalPat, liveEvalSoundPat)
import Minipat.Dirt.Osc (Attrs)
import Minipat.Stream (Stream (..), streamInnerBind)

setIn, (#) :: Stream Attrs -> Stream Attrs -> Stream Attrs
setIn p1 p2 = streamInnerBind p1 (\m1 -> fmap (m1 <>) p2)
(#) = setIn

pat :: DatumType -> Text -> Text -> Stream Attrs
pat dt k t = stream k (liveEvalPat dt t)

stream :: (IsDatum a) => Text -> Stream a -> Stream Attrs
stream k = fmap (Map.singleton k . toDatum)

sound, s :: Text -> Stream Attrs
sound = liveEvalSoundPat
s = sound

note, n :: Text -> Stream Attrs
note = pat DatumTypeInt32 "note"
n = note
