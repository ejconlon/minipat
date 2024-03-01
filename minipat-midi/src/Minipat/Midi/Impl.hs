{-# LANGUAGE OverloadedStrings #-}

-- | Live MIDI backend implementation
module Minipat.Midi.Impl
  ( MidiBackend (..)
  , MidiSt
  )
where

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Dahdit.Iface (mutEncode)
import Dahdit.Midi.Midi (ChanData (..), ChanVoiceData (..), Channel, ShortMsg (..))
import Data.Acquire (mkAcquire)
import Data.Default (Default (..))
import Data.Foldable (foldl')
import Data.Heap (Heap)
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
import Minipat.Midi.Convert (convertMidiAttrs)
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

-- | We order so that note offs come before other messages
newtype SortedMsg = SortedMsg {unSortedMsg :: ShortMsg}
  deriving stock (Show)
  deriving newtype (Eq)

isNoteOff :: ShortMsg -> Bool
isNoteOff = \case
  ShortMsgChan _ (ChanDataVoice cvd) ->
    case cvd of
      ChanVoiceDataNoteOn _ 0 -> True
      ChanVoiceDataNoteOff _ _ -> True
      _ -> False
  _ -> False

instance Ord SortedMsg where
  compare (SortedMsg m1) (SortedMsg m2) =
    let o1 = isNoteOff m1
        o2 = isNoteOff m2
        r = compare m1 m2
    in  if o1
          then
            if o2
              then r
              else LT
          else
            if o2
              then GT
              else r

data TimedMsg = TimedMsg
  { tmTime :: !PosixTime
  , tmMsg :: !SortedMsg
  }
  deriving stock (Eq, Ord, Show)

mkNoteOff :: Channel -> ChanData -> Maybe ShortMsg
mkNoteOff c = \case
  ChanDataVoice (ChanVoiceDataNoteOn n v)
    | v > 0 ->
        Just (ShortMsgChan c (ChanDataVoice (ChanVoiceDataNoteOn n 0)))
  _ -> Nothing

mkTimedMsgs :: WithPlayMeta ChanData -> Seq TimedMsg
mkTimedMsgs (WithPlayMeta pm cd) =
  let Arc t1 t2 = pmRealArc pm
      c = fromInteger (pmOrbit pm)
      m1 = ShortMsgChan c cd
      s1 = Seq.singleton (TimedMsg t1 (SortedMsg m1))
  in  case mkNoteOff c cd of
        Just m2 -> s1 :|> TimedMsg t2 (SortedMsg m2)
        Nothing -> s1

data MidiData = MidiData
  { mdDevice :: !OutputDevice
  , mdHeap :: !(TVar (Heap TimedMsg))
  , mdSendTask :: !(Async ())
  }

instance Backend MidiBackend where
  type BackendData MidiBackend = MidiData

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
    let send (TimedMsg _ (SortedMsg m)) = do
          len <- fmap fromIntegral (mutEncode m buf)
          VSM.unsafeWith buf (\ptr -> R.sendUnsafeMessage device ptr len)
    fmap (MidiData device heap) (acquireAwait tmTime getPlayingSTM (qhHeap heap) send)

  backendSend _ _ cb evs = runCallback cb $ \md -> do
    msgs <- either throwIO pure (traverse (traverse convertMidiAttrs) evs)
    let timedMsgs = msgs >>= mkTimedMsgs
    atomically (modifyTVar' (mdHeap md) (\h -> foldl' (flip H.insert) h timedMsgs))

  backendClear _ _ cb = runCallback cb $ \md ->
    atomically (writeTVar (mdHeap md) H.empty)

  backendCheck _ logger cb = runCallback cb $ \md -> do
    h <- readTVarIO (mdHeap md)
    logInfo logger ("Queue length: " <> T.pack (show (H.size h)))
    case H.uncons h of
      Just (TimedMsg t (SortedMsg m), _) -> logInfo logger ("Queue head: " <> T.pack (show t) <> " " <> T.pack (show m))
      Nothing -> pure ()
    logAsyncState logger "send" (mdSendTask md)
