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
  , PortName
  , port
  , midi
  , module Minipat.Live.Boot
  )
where

import Minipat.Midi.Midi (PortMsg)
import Data.Sequence (Seq)
import Data.Text (Text)
import Minipat.Live.Boot
import Minipat.Live.Datum (DatumProxy (..))
import Minipat.Live.Extra (Note, parseDatum, parseMidiNote, parseNote)
import Minipat.Midi.Convert (Vel (..))
import Minipat.Midi.Impl qualified as I
import Minipat.Midi.Midi (PortName (..))

type MidiLiveSt = (LiveSt, LiveBackend ~ I.MidiBackend)

note, n :: Text -> S Note
note = parseNote
n = note

midinote :: Text -> S Note
midinote = parseMidiNote

vel, v :: Text -> S Vel
vel = fmap Vel . parseDatum DatumProxyInt32
v = vel

port :: Text -> S PortName
port = fmap PortName . parseDatum DatumProxyString

midi :: (MidiLiveSt) => Seq PortMsg -> IO ()
midi ms = readLiveSt >>= \st -> I.sendMsgs st ms
