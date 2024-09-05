{-# LANGUAGE OverloadedStrings #-}

-- | Live MIDI backend implementation
module Minipat.Midi.Impl
  ( MidiBackend (..)
  , MidiSt
  , sendMsgs
  , sendLiveMsgs
  , connectAndSendMsgs
  )
where

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Control.Exception (finally, throwIO)
import Control.Monad.IO.Class (liftIO)
import Dahdit.Iface (mutEncode)
import Dahdit.Midi.Midi (ChanData (..), ChanVoiceData (..), Channel, LiveMsg (..))
import Data.Acquire (mkAcquire)
import Data.Default (Default (..))
import Data.Foldable (foldl', for_)
import Data.Heap (Heap)
import Data.Heap qualified as H
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Vector.Storable.Mutable qualified as VSM
import Minipat.Live.Backend (Backend (..), Callback (..), PlayMeta (..), WithPlayMeta (..))
import Minipat.Live.Core (St, logAsyncState, stBackend, useCallback)
import Minipat.Live.Logger (logInfo)
import Minipat.Live.Resources (acquireAwait, qhHeap)
import Minipat.Midi.Convert (convertMidiAttrs)
import Minipat.Midi.Midi (MidiState (..), SortedMsg (..), TimedMsg (..), PortName, PortMsg, MidiEnv (..), PortData)
import Minipat.Time (Arc (..))
import Nanotime (PosixTime, TimeDelta, threadDelayDelta)
import Libremidi.Api (Api (..))

defaultMaxMsgLen :: Int
defaultMaxMsgLen = 1024

data MidiBackend = MidiBackend
  { mbApi :: !Api
  , mbMaxMsgLen :: !Int
  , mbDelay :: !(Maybe TimeDelta)
  }

instance Default MidiBackend where
  def = MidiBackend ApiUnspecified defaultMaxMsgLen Nothing

type MidiSt = St MidiBackend

mkTimedMsgs :: WithPlayMeta PortData -> Seq TimedMsg
mkTimedMsgs (WithPlayMeta pm pd) =
  let Arc t1 t2 = pmRealArc pm
      c = fromInteger (pmOrbit pm - 1)
      m1 = LiveMsgChan c cd
      s1 = Seq.singleton (TimedMsg t1 (SortedMsg m1))
  in  case mkNoteOff c pd of
        Just m2 -> s1 :|> TimedMsg t2 (SortedMsg m2)
        Nothing -> s1

data MidiData = MidiData
  { mdMidiEnv :: !MidiEnv
  , mdObsTask :: !(Async ())
  , mdSendTask :: !(Async ())
  }

-- sendMsgs :: (Foldable f) => St MidiBackend -> f PortMsg -> IO ()
-- sendMsgs st msgs = useCallback st $ \md ->
--   let MidiBackend _ _ maxLen mayDelay = stBackend st
--   in  sendLiveMsgs maxLen mayDelay msgs
--
-- connectAndSendMsgs :: (Foldable f) => MidiBackend -> f PortMsg -> IO ()
-- connectAndSendMsgs (MidiBackend openPred defPred maxLen mayDelay) msgs = do
--   device <- liftIO R.defaultOutput
--   mp <- R.findPort device portSel
--   case mp of
--     Nothing -> fail "Could not find acceptable port"
--     Just p -> do
--       flip finally (R.closePort device) $ do
--         R.openPort device p "minipat"
--         sendLiveMsgs maxLen mayDelay device msgs

instance Backend MidiBackend where
  type BackendData MidiBackend = MidiData

  backendInit (MidiBackend api maxLen mayDelay) logger getPlayingSTM = do
    -- device <- liftIO R.defaultOutput
    -- let getPort = do
    --       mp <- R.findPort device portSel
    --       case mp of
    --         Nothing -> fail "Could not find acceptable port"
    --         Just p -> do
    --           name <- fmap (fromMaybe "UNNAMED") (R.portName device p)
    --           logInfo logger ("Opening port" <> T.pack (show p) <> " (" <> T.pack name <> ")")
    --           R.openPort device p "minipat"
    --           logInfo logger "Connected"
    -- _ <- mkAcquire getPort (const (R.closePort device))
    buf <- liftIO (VSM.new maxLen)
    ms <- newMidiState
    let me = MidiEnv api ms
    let send (TimedMsg _ (SortedMsg m)) = do
          len <- fmap fromIntegral (mutEncode m buf)
          VSM.unsafeWith buf (\ptr -> R.sendUnsafeMessage device ptr len)
          for_ mayDelay threadDelayDelta
    sendTask <- acquireAwait tmTime getPlayingSTM (qhHeap heap) send
    fmap (MidiData device heap)

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
