{-# LANGUAGE OverloadedStrings #-}

module Minipat.Midi.Impl where

-- TODO explicit exports

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Dahdit.Iface (mutEncode)
import Dahdit.Midi.Midi (ChanData (..), ChanVoiceData (..), ShortMsg (..))
import Data.Acquire (mkAcquire)
import Data.Default (Default (..))
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
import Minipat.Live.Core (St, logAsyncState)
import Minipat.Live.Logger (logInfo)
import Minipat.Live.Resources (acquireAwait, qhHeap)
import Minipat.Time (Arc (..))
import Nanotime (PosixTime)
import Sound.RtMidi (OutputDevice)
import Sound.RtMidi qualified as R

newtype MidiBackend = MidiBackend
  { mbPortSel :: String -> Bool
  }

instance Default MidiBackend where
  def = MidiBackend (const True)

type MidiSt = St MidiBackend

data MidiData = MidiData
  { mdDevice :: !OutputDevice
  , mdHeap :: !(TVar (Heap (Entry PosixTime ShortMsg)))
  , mdSendTask :: !(Async ())
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
              logInfo logger ("Opening port" <> T.pack (show p) <> " (" <> T.pack name <> ")")
              R.openPort device p "minipat"
              logInfo logger "Connected"
    _ <- mkAcquire getPort (const (R.closePort device))
    heap <- liftIO (newTVarIO H.empty)
    buf <- liftIO (VSM.new 4)
    let send (Entry _ m) = do
          len <- fmap fromIntegral (mutEncode m buf)
          VSM.unsafeWith buf (\ptr -> R.sendUnsafeMessage device ptr len)
    fmap (MidiData device heap) (acquireAwait H.priority getPlayingSTM (qhHeap heap) send)

  backendSend _ _ cb evs = runCallback cb $ \md ->
    let entries = evs >>= mkMsgs <&> uncurry Entry
    in  atomically (modifyTVar' (mdHeap md) (\h -> foldl' (flip H.insert) h entries))

  backendClear _ _ cb = runCallback cb $ \md ->
    atomically (writeTVar (mdHeap md) H.empty)

  backendCheck _ logger cb = runCallback cb $ \md -> do
    h <- readTVarIO (mdHeap md)
    logInfo logger ("Queue length: " <> T.pack (show (H.size h)))
    case H.uncons h of
      Just (Entry t m, _) -> logInfo logger ("Queue head: " <> T.pack (show t) <> " " <> T.pack (show m))
      Nothing -> pure ()
    logAsyncState logger "send" (mdSendTask md)
