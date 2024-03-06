module Minipat.Dirt.Boot
  ( I.DirtBackend
  , I.DirtSt
  , DirtLiveSt
  , sound
  , s
  , note
  , n
  , midinote
  , module Minipat.Dirt.Params
  , module Minipat.Live.Boot
  , module Minipat.Live.Extra
  )
where

import Data.Text (Text)
import Minipat.Dirt.Impl qualified as I
import Minipat.Dirt.Params
import Minipat.Live.Boot
import Minipat.Live.Extra

type DirtLiveSt = (LiveSt, LiveBackend ~ I.DirtBackend)

sound, s :: Text -> S Sound
sound = parseSound
s = sound

note, n :: Text -> S Note
note = parseNote
n = note

midinote :: Text -> S Note
midinote = parseMidiNote
