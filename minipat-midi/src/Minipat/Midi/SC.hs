{-# LANGUAGE OverloadedLists #-}

-- | Midi message makers for SC-88 and the like
module Minipat.Midi.SC where

import Dahdit.Midi.Midi (ChanData (..), ChanVoiceData (..), ShortMsg (..))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

setControl :: Int -> Int -> Int -> Seq ShortMsg
setControl part control value =
  Seq.singleton
    ( ShortMsgChan
        (fromIntegral part)
        ( ChanDataVoice
            ( ChanVoiceControlChange
                (fromIntegral control)
                (fromIntegral value)
            )
        )
    )

setProgram :: Int -> Int -> Seq ShortMsg
setProgram part program =
  Seq.singleton
    ( ShortMsgChan
        (fromIntegral part)
        ( ChanDataVoice
            ( ChanVoiceProgramChange
                (fromIntegral program)
            )
        )
    )

setSound :: Int -> Int -> Int -> Seq ShortMsg
setSound part inst var =
  mconcat
    [ setControl part 0 (var * 8)
    , setControl part 32 0
    , setProgram part inst
    ]

setLevel :: Int -> Int -> Seq ShortMsg
setLevel = flip setControl 7

setPan :: Int -> Int -> Seq ShortMsg
setPan = flip setControl 10

setReverb :: Int -> Int -> Seq ShortMsg
setReverb = flip setControl 91

setChorus :: Int -> Int -> Seq ShortMsg
setChorus = flip setControl 93

allSoundsOff :: Int -> Seq ShortMsg
allSoundsOff part = setControl part 120 0

-- Turn of non-sustained notes
allNotesOff :: Int -> Seq ShortMsg
allNotesOff part = setControl part 123 0

reinit :: Int -> Seq ShortMsg
reinit part =
  mconcat
    [ setSound part 0 0
    , setLevel part 100
    , setPan part 64
    , setReverb part 40
    , setChorus part 0
    ]
