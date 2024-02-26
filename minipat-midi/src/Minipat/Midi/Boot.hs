{-# LANGUAGE OverloadedStrings #-}

module Minipat.Midi.Boot
  ( M.MidiBackend
  , M.MidiSt
  , MidiLiveSt
  , initialize
  , module Minipat.Live.Boot
  )
where

import Minipat.Live.Boot
import Minipat.Live.Core qualified as C
import Minipat.Live.Logger qualified as L
import Minipat.Midi.Impl qualified as M

type MidiLiveSt = (LiveSt, LiveBackend ~ M.MidiBackend)

initialize :: IO M.MidiSt
initialize = do
  logger <- L.newLogger
  L.logInfo logger "Initializing"
  C.initAsyncSt logger M.defaultMidiBackend C.defaultEnv
