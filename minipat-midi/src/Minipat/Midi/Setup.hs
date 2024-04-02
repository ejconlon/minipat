{-# LANGUAGE OverloadedStrings #-}

-- Setup that makes sense for me...
module Minipat.Midi.Setup where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Dahdit (StaticSeq (..))
import Dahdit.Midi.Midi (LiveMsg)
import Data.Default (def)
import Data.List (isPrefixOf)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Minipat.Midi.Impl (MidiBackend (..), connectAndSendMsgs)
import Minipat.Midi.Mpk
import Minipat.Midi.SC
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

mpkBackend :: MidiBackend
mpkBackend =
  def
    { mbPortSel = isPrefixOf "MPK mini"
    , mbDelay = Just (timeDeltaFromFracSecs @Double 0.5)
    }

withMpk :: Seq LiveMsg -> IO ()
withMpk = connectAndSendMsgs mpkBackend

scBackend :: MidiBackend
scBackend =
  def
    { mbPortSel = isPrefixOf "U2MIDI Pro"
    , mbDelay = Just (timeDeltaFromFracSecs @Double 0.05)
    }

withSc :: Seq LiveMsg -> IO ()
withSc = connectAndSendMsgs scBackend

sendCfgs :: IO ()
sendCfgs = withMpk (Seq.fromList [sendProgConfig (ProgBank i) (mkCfg i) | i <- [0 .. 7]])

type InstVar = TVar Inst

newInstVar :: IO InstVar
newInstVar = newTVarIO firstInstrument

findInstVar :: Text -> InstVar -> IO ()
findInstVar t v = do
  i <- maybe (error ("Inst not found: " ++ T.unpack t)) pure (findInstrument t)
  atomically (writeTVar v i)

nextInstVar :: InstVar -> IO ()
nextInstVar v = atomically (modifyTVar' v nextInstrument)

sendInstVar :: Int -> InstVar -> IO ()
sendInstVar chan v = do
  Inst _ prog var _ <- readTVarIO v
  withSc (setSound chan prog var)

reinitAll :: IO ()
reinitAll = withSc $ do
  chan <- Seq.fromList [0 .. 15]
  mconcat
    [ allSoundsOff chan
    , setSound chan 0 0
    , reinit chan
    ]
