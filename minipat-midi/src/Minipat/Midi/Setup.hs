{-# LANGUAGE OverloadedStrings #-}

-- Setup that makes sense for me...
module Minipat.Midi.Setup where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad (unless)
import Dahdit (StaticSeq (..))
import Dahdit.Midi.Midi (LiveMsg)
import Data.Default (def)
import Data.Foldable (toList)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Minipat.Midi.Impl (MidiBackend (..), connectAndSendMsgs)
import Minipat.Midi.Mpk
import Minipat.Midi.SC
import Nanotime (timeDeltaFromFracSecs)

-- Mpk config for Sc control
mkMpkCfg :: Int -> ProgConfig
mkMpkCfg i = c
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

-- Send mpk config
sendMpkCfgs :: IO ()
sendMpkCfgs = withMpk (Seq.fromList [sendProgConfig (ProgBank i) (mkMpkCfg i) | i <- [0 .. 7]])

midiChannels :: Seq Int
midiChannels = Seq.fromList [0 .. 15]

guardChannel :: (MonadFail m) => Int -> m ()
guardChannel c = unless (c >= 0 && c < 16) (fail ("Invalid MIDI channel: " ++ show c))

type ScConf = Map Int Inst

newScConf :: ScConf
newScConf = Map.fromList (fmap (,firstInstrument) (toList midiChannels))

getInst :: Int -> ScVar -> IO Inst
getInst chan v = do
  guardChannel chan
  fmap (Map.! chan) (readTVarIO v)

findInst :: Int -> Text -> ScVar -> IO ()
findInst chan t v = do
  guardChannel chan
  i <- maybe (fail ("Inst not found: " ++ T.unpack t)) pure (findInstrument t)
  atomically (modifyTVar' v (Map.insert chan i))

nextInst :: Int -> ScVar -> IO ()
nextInst chan v = do
  guardChannel chan
  atomically (modifyTVar' v (Map.adjust nextInstrument chan))

sendInst :: Int -> ScVar -> IO ()
sendInst chan v = do
  guardChannel chan
  Inst _ prog variant _ <- fmap (Map.! chan) (readTVarIO v)
  withSc (setSound chan prog variant)

sendAllInsts :: ScVar -> IO ()
sendAllInsts v = do
  conf <- readTVarIO v
  let msgs = do
        (chan, Inst _ prog variant _) <- Seq.fromList (Map.toList conf)
        setSound chan prog variant
  withSc msgs

type ScVar = TVar ScConf

newScVar :: IO ScVar
newScVar = newTVarIO newScConf

reinitSc :: IO ()
reinitSc = withSc $ do
  chan <- midiChannels
  mconcat
    [ allSoundsOff chan
    , setSound chan 0 0
    , reinit chan
    ]
