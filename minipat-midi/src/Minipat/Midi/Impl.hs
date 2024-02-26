{-# LANGUAGE OverloadedStrings #-}

module Minipat.Midi.Impl where

-- TODO explicit exports

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Dahdit.Iface (mutEncode)
import Dahdit.Midi.Midi (ChanData (..), ChanVoiceData (..), ShortMsg (..))
import Data.Acquire (mkAcquire)
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.Heap (Entry (..), Heap)
import Data.Heap qualified as H
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Vector.Storable.Mutable qualified as VSM
import Minipat.Live.Backend (Backend (..), Callback (..), PlayMeta (..), WithPlayMeta (..))
import Minipat.Live.Core (St)
import Minipat.Live.Logger (logInfo)
import Minipat.Live.Resources (acquireAwait, qhHeap)
import Minipat.Time (Arc (..))
import Nanotime (PosixTime)
import Sound.RtMidi (OutputDevice)
import Sound.RtMidi qualified as R

newtype MidiBackend = MidiBackend
  { mbPortSel :: String -> Bool
  }

defaultMidiBackend :: MidiBackend
defaultMidiBackend = MidiBackend (const True)

type MidiSt = St MidiBackend

data MidiData = MidiData
  { mdDevice :: !OutputDevice
  , mdHeap :: !(TVar (Heap (Entry PosixTime ShortMsg)))
  }

mkMsgs :: WithPlayMeta ChanData -> Seq (PosixTime, ShortMsg)
mkMsgs (WithPlayMeta pm cd) =
  let Arc t1 t2 = pmRealArc pm
      c = fromInteger (pmOrbit pm)
      m1 = ShortMsgChan c cd
      s1 = Seq.singleton (t1, m1)
  in  case cd of
        ChanDataVoice cvd -> case cvd of
          ChanVoiceDataNoteOn n v
            | v > 0 ->
                let m2 = ShortMsgChan c (ChanDataVoice (ChanVoiceDataNoteOn n 0))
                in  s1 :|> (t2, m2)
          _ -> s1
        _ -> s1

instance Backend MidiBackend where
  type BackendData MidiBackend = MidiData
  type BackendAttrs MidiBackend = ChanData

  backendInit (MidiBackend portSel) logger getPlayingSTM = do
    device <- liftIO R.defaultOutput
    let getPort = do
          mp <- R.findPort device portSel
          case mp of
            Nothing -> fail "Could not find acceptable port"
            Just p -> do
              name <- fmap (fromMaybe "UNNAMED") (R.portName device p)
              logInfo logger ("Opened port " <> T.pack (show p) <> " (" <> T.pack name <> ")")
    _ <- mkAcquire getPort (const (R.closePort device))
    heap <- liftIO (newTVarIO H.empty)
    buf <- liftIO (VSM.new 4)
    let send (Entry _ m) = do
          len <- fmap fromIntegral (mutEncode m buf)
          VSM.unsafeWith buf (\ptr -> R.sendUnsafeMessage device ptr len)
    _ <- acquireAwait H.priority getPlayingSTM (qhHeap heap) send
    pure (MidiData device heap)

  backendSend _ _ cb evs = runCallback cb $ \md ->
    let entries = evs >>= mkMsgs <&> uncurry Entry
    in  atomically (modifyTVar' (mdHeap md) (\h -> foldl' (flip H.insert) h entries))

  backendClear _ _ cb = runCallback cb $ \md ->
    atomically (writeTVar (mdHeap md) H.empty)

  -- TODO really check
  backendCheck _ _ _ = pure True
