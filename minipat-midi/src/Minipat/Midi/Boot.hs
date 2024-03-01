module Minipat.Midi.Boot
  ( I.MidiBackend
  , I.MidiSt
  , MidiLiveSt
  , MidiNote
  , note
  , n
  , Vel
  , vel
  , v
  , module Minipat.Live.Boot
  )
where

import Data.Text (Text)
import Minipat.Live.Boot
import Minipat.Live.Datum (DatumProxy (..))
import Minipat.Live.Extra (MidiNote, parseDatum, parseMidiNote)
import Minipat.Midi.Convert (Vel (..))
import Minipat.Midi.Impl qualified as I

type MidiLiveSt = (LiveSt, LiveBackend ~ I.MidiBackend)

note, n :: Text -> S MidiNote
note = parseMidiNote
n = note

vel, v :: Text -> S Vel
vel = fmap Vel . parseDatum DatumProxyInt32
v = vel
