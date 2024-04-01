module Minipat.Midi.Boot
  ( I.MidiBackend
  , I.MidiSt
  , MidiLiveSt
  , note
  , n
  , midinote
  , Vel
  , vel
  , v
  , midi
  , module Minipat.Live.Boot
  )
where

import Dahdit.Midi.Midi (LiveMsg)
import Data.Sequence (Seq)
import Data.Text (Text)
import Minipat.Live.Boot
import Minipat.Live.Datum (DatumProxy (..))
import Minipat.Live.Extra (Note, parseDatum, parseMidiNote, parseNote)
import Minipat.Midi.Convert (Vel (..))
import Minipat.Midi.Impl qualified as I

type MidiLiveSt = (LiveSt, LiveBackend ~ I.MidiBackend)

note, n :: Text -> S Note
note = parseNote
n = note

midinote :: Text -> S Note
midinote = parseMidiNote

vel, v :: Text -> S Vel
vel = fmap Vel . parseDatum DatumProxyInt32
v = vel

midi :: (MidiLiveSt) => Seq LiveMsg -> IO ()
midi ms = readLiveSt >>= \st -> I.sendMsgs st ms
