{-# LANGUAGE OverloadedStrings #-}

-- | Live MIDI backend implementation
module Minipat.Midi.Impl
  ( MidiBackend (..)
  , MidiSt
  , openOutPort
  , setDefaultOutPort
  , sendMsgs
  , connectAndSendMsgs
  )
where

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Dahdit.Midi.Midi (LiveMsg (..))
import Data.Acquire (withAcquire)
import Data.Default (Default (..))
import Data.Foldable (foldl', for_, toList)
import Data.Heap (Heap)
import Data.Heap qualified as H
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Storable.Mutable qualified as VSM
import Libremidi.Api (Api (..))
import Minipat.Live.Backend (Backend (..), Callback (..), PlayMeta (..), WithPlayMeta (..))
import Minipat.Live.Core (St, logAsyncState, stBackend, useCallback)
import Minipat.Live.Logger (logDebug, logInfo, newLogger)
import Minipat.Live.Resources (acquireAsync, acquireAwait, acquirePure, qhHeap)
import Minipat.Midi.Convert (convertMidiAttrs)
import Minipat.Midi.Count (execCountM)
import Minipat.Midi.Midi qualified as M
import Minipat.Time (Arc (..))
import Nanotime (TimeDelta, threadDelayDelta)

defaultMaxMsgLen :: Int
defaultMaxMsgLen = 1024

data MidiBackend = MidiBackend
  { mbApi :: !Api
  , mbAutoConn :: !M.AutoConn
  , mbDefOut :: !(Maybe Text)
  , mbMaxMsgLen :: !Int
  , mbDelay :: !(Maybe TimeDelta)
  }

instance Default MidiBackend where
  def = MidiBackend ApiUnspecified M.AutoConnYes Nothing defaultMaxMsgLen Nothing

type MidiSt = St MidiBackend

mkTimedMsgs :: WithPlayMeta M.PortData -> Seq M.TimedMsg
mkTimedMsgs (WithPlayMeta pm pd@(M.PortData ps cd)) =
  let Arc t1 t2 = pmRealArc pm
      c = fromInteger (pmOrbit pm - 1)
      m1 = M.PortMsg ps (LiveMsgChan c cd)
      s1 = Seq.singleton (M.TimedMsg t1 (M.SortedMsg m1))
  in  case M.mkNoteOff c pd of
        Just m2 -> s1 :|> M.TimedMsg t2 (M.SortedMsg m2)
        Nothing -> s1

data MidiData = MidiData
  { mdEnv :: !M.MidiEnv
  , mdHeap :: !(TVar (Heap M.TimedMsg))
  , mdConnTask :: !(Async ())
  , mdSendTask :: !(Async ())
  }

openOutPort :: MidiSt -> M.PortSel -> IO ()
openOutPort st ps = useCallback st $ \md -> do
  execCountM (M.openOutPort ps M.SetDefaultNo) (mdEnv md)

setDefaultOutPort :: MidiSt -> M.PortSel -> IO ()
setDefaultOutPort st ps = useCallback st $ \md -> do
  execCountM (M.setDefaultOutPort ps) (mdEnv md)

sendMsgs :: (Foldable f) => MidiSt -> f M.PortMsg -> IO ()
sendMsgs st msgs = useCallback st $ \md -> do
  let MidiBackend _ ac _ maxLen mayDelay = stBackend st
  execCountM (M.sendPortMsgs maxLen mayDelay ac msgs) (mdEnv md)

connectAndSendMsgs :: (Foldable f) => MidiBackend -> f LiveMsg -> IO ()
connectAndSendMsgs mb@(MidiBackend _ _ defOut maxLen mayDelay) msgs = withAcquire acq use
 where
  ps = M.PortSelPrefix (fromMaybe "" defOut)
  acq = do
    logger <- liftIO newLogger
    backendInit mb logger (pure False)
  use md =
    let msgs' = fmap (M.PortMsg ps) (toList msgs)
    in  execCountM (M.sendPortMsgs maxLen mayDelay M.AutoConnNo msgs') (mdEnv md)

instance Backend MidiBackend where
  type BackendData MidiBackend = MidiData

  backendInit (MidiBackend api ac defOut maxLen mayDelay) logger getPlayingSTM = do
    liftIO (logDebug logger "Initializing midi backend")
    sendHeap <- liftIO (newTVarIO H.empty)
    buf <- liftIO (VSM.new maxLen)
    ms <- liftIO M.newMidiState
    let me = M.MidiEnv api logger ms
        send (M.TimedMsg _ (M.SortedMsg pm)) = do
          execCountM (M.sendPortMsg buf ac pm) me
          for_ mayDelay threadDelayDelta
        spawn = acquireAwait M.tmTime getPlayingSTM
    connTask <- case defOut of
      Nothing -> acquirePure ()
      Just t -> acquireAsync $ do
        logDebug logger ("Opening default output: " <> t)
        execCountM (M.openOutPort (M.PortSelPrefix t) M.SetDefaultYes) me
    sendTask <- spawn (qhHeap sendHeap) send
    liftIO (logDebug logger "Initialization complete")
    pure (MidiData me sendHeap connTask sendTask)

  backendSend _ _ cb evs = runCallback cb $ \md -> do
    msgs <- either throwIO pure (traverse (traverse convertMidiAttrs) evs)
    let timedMsgs = msgs >>= mkTimedMsgs
    atomically (modifyTVar' (mdHeap md) (\h -> foldl' (flip H.insert) h timedMsgs))

  backendClear _ _ cb = runCallback cb $ \md ->
    atomically (writeTVar (mdHeap md) H.empty)

  backendCheck _ logger cb = runCallback cb $ \md -> do
    -- Show task info
    connOk <- logAsyncState logger "conn" (mdConnTask md)
    sendOk <- logAsyncState logger "send" (mdSendTask md)
    -- Show ports
    let ms = M.meState (mdEnv md)
    (mayDefOut, outPorts) <- atomically $ do
      mayDefOut <- fmap (fmap M.unPortName) (readTVar (M.msOutDefault ms))
      outMap <- readTVar (M.msOutMap ms)
      let outPorts = fmap M.unPortName (toList (Map.keys outMap))
      pure (mayDefOut, outPorts)
    let outOk = not (null outPorts)
    if outOk
      then do
        logInfo logger "Out ports:"
        for_ outPorts $ \op ->
          logInfo logger $
            if mayDefOut == Just op
              then op <> " (default)"
              else op
      else logInfo logger "No out ports"
    -- Show queue info
    h <- readTVarIO (mdHeap md)
    logInfo logger ("Queue length: " <> T.pack (show (H.size h)))
    case H.uncons h of
      Just (M.TimedMsg t (M.SortedMsg m), _) ->
        logInfo logger ("Queue head: " <> T.pack (show t) <> " " <> T.pack (show m))
      Nothing -> pure ()
    pure (connOk && sendOk && outOk)
