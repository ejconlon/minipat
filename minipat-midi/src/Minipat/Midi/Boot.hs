module Minipat.Midi.Boot
  ( I.MidiBackend
  , I.MidiSt
  , MidiLiveSt
  , module Minipat.Live.Boot
  )
where

import Minipat.Live.Boot
import Minipat.Midi.Impl qualified as I

type MidiLiveSt = (LiveSt, LiveBackend ~ I.MidiBackend)
