{-# LANGUAGE OverloadedStrings #-}

-- | Live MIDI backend implementation
module Minipat.Midi.Impl
  ( MidiBackend (..)
  , MidiSt
  , sendMsgs
  , connectAndSendMsgs
  )
where

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Dahdit.Midi.Midi (LiveMsg (..))
import Data.Acquire (withAcquire)
import Data.Default (Default (..))
import Data.Foldable (foldl', for_, toList)
import Data.Heap (Heap)
import Data.Heap qualified as H
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Storable.Mutable qualified as VSM
import Libremidi.Api (Api (..))
import Minipat.Live.Backend (Backend (..), Callback (..), PlayMeta (..), WithPlayMeta (..))
import Minipat.Live.Core (St, logAsyncState, stBackend, useCallback)
import Minipat.Live.Logger (logInfo, newLogger)
import Minipat.Live.Resources (acquireAsync, acquireAwait, acquirePure, qhHeap)
import Minipat.Midi.Convert (convertMidiAttrs)
import Minipat.Midi.Count (execCountM)
import Minipat.Midi.Midi
  ( AutoConn (..)
  , MidiEnv (..)
  , PortData (..)
  , PortMsg (..)
  , PortSel (..)
  , SetDefault (..)
  , SortedMsg (..)
  , TimedMsg (..)
  , mkNoteOff
  , newMidiState
  , openOutPort
  , sendPortMsg'
  , sendPortMsgs
  )
import Minipat.Time (Arc (..))
import Nanotime (TimeDelta, threadDelayDelta)

defaultMaxMsgLen :: Int
defaultMaxMsgLen = 1024

data MidiBackend = MidiBackend
  { mbApi :: !Api
  , mbAutoConn :: !AutoConn
  , mbDefOut :: !(Maybe Text)
  , mbMaxMsgLen :: !Int
  , mbDelay :: !(Maybe TimeDelta)
  }

instance Default MidiBackend where
  def = MidiBackend ApiUnspecified AutoConnYes Nothing defaultMaxMsgLen Nothing

type MidiSt = St MidiBackend

mkTimedMsgs :: WithPlayMeta PortData -> Seq TimedMsg
mkTimedMsgs (WithPlayMeta pm pd@(PortData ps cd)) =
  let Arc t1 t2 = pmRealArc pm
      c = fromInteger (pmOrbit pm - 1)
      m1 = PortMsg ps (LiveMsgChan c cd)
      s1 = Seq.singleton (TimedMsg t1 (SortedMsg m1))
  in  case mkNoteOff c pd of
        Just m2 -> s1 :|> TimedMsg t2 (SortedMsg m2)
        Nothing -> s1

data MidiData = MidiData
  { mdEnv :: !MidiEnv
  , mdHeap :: !(TVar (Heap TimedMsg))
  , mdConnTask :: !(Async ())
  , mdSendTask :: !(Async ())
  }

sendMsgs :: (Foldable f) => MidiSt -> f PortMsg -> IO ()
sendMsgs st msgs = useCallback st $ \md -> do
  let MidiBackend _ ac _ maxLen mayDelay = stBackend st
  execCountM (sendPortMsgs maxLen mayDelay ac msgs) (mdEnv md)

connectAndSendMsgs :: (Foldable f) => MidiBackend -> f LiveMsg -> IO ()
connectAndSendMsgs mb@(MidiBackend _ _ defOut maxLen mayDelay) msgs = withAcquire acq use
 where
  ps = PortSelPrefix (fromMaybe "" defOut)
  acq = do
    logger <- liftIO newLogger
    backendInit mb logger (pure False)
  use md =
    let msgs' = fmap (PortMsg ps) (toList msgs)
    in  execCountM (sendPortMsgs maxLen mayDelay AutoConnNo msgs') (mdEnv md)

instance Backend MidiBackend where
  type BackendData MidiBackend = MidiData

  backendInit (MidiBackend api ac defOut maxLen mayDelay) logger getPlayingSTM = do
    sendHeap <- liftIO (newTVarIO H.empty)
    buf <- liftIO (VSM.new maxLen)
    ms <- liftIO newMidiState
    let me = MidiEnv api logger ms
        send (TimedMsg _ (SortedMsg pm)) = do
          execCountM (sendPortMsg' buf ac pm) me
          for_ mayDelay threadDelayDelta
        spawn = acquireAwait tmTime getPlayingSTM
    connTask <- case defOut of
      Nothing -> acquirePure ()
      Just t -> acquireAsync (execCountM (openOutPort (PortSelPrefix t) SetDefaultYes) me)
    sendTask <- spawn (qhHeap sendHeap) send
    pure (MidiData me sendHeap connTask sendTask)

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
