{-# LANGUAGE OverloadedStrings #-}

module Minipat.Midi.Setup where

import Dahdit (StaticSeq (..))
import Data.Default (def)
import Data.List (isPrefixOf)
import Data.Sequence qualified as Seq
import Data.String (fromString)
import Minipat.Midi.Impl (MidiBackend (..), connectAndSendMsgs)
import Minipat.Midi.Mpk
import Nanotime (timeDeltaFromFracSecs)

mkCfg :: Int -> ProgConfig
mkCfg i = c
 where
  k name cc = Knob KnobModeAbs cc 0 127 name
  c =
    def
      { pcName = fromString ("PGM:" ++ show i)
      , pcPadMidiChan = fromIntegral i
      , pcKeyMidiChan = fromIntegral i
      , pcPadBankA =
          StaticSeq (Seq.fromList [Pad (60 + j) j j | j <- [0 .. 7]])
      , pcKnobs =
          StaticSeq $
            Seq.fromList
              [ k "Modulation" 1
              , k "Portamento" 5
              , k "Volume" 7
              , k "Pan" 10
              , k "Expression" 11
              , k "Reverb" 91
              , k "Chorus" 93
              , k "Delay" 94
              ]
      }

sendCfgs :: IO ()
sendCfgs =
  let mb =
        def
          { mbPortSel = isPrefixOf "MPK mini"
          , mbDelay = Just (timeDeltaFromFracSecs @Double 0.5)
          }
      ms = [sendProgConfig (ProgBank i) (mkCfg i) | i <- [0 .. 7]]
  in  connectAndSendMsgs mb ms
