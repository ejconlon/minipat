{-# LANGUAGE OverloadedStrings #-}

module Minipat.Midi.Midi where

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO, stateTVar, writeTVar)
import Control.Exception (Exception)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import Dahdit.Iface (mutEncode)
import Dahdit.Midi.Midi (ChanData (..), ChanVoiceData (..), Channel, LiveMsg (..))
import Dahdit.Midi.Osc (Datum (..))
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Default (Default (..))
import Data.Foldable (for_)
import Data.Heap (Heap)
import Data.Heap qualified as Heap
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word (Word8)
import Libremidi.Api
  ( Api
  , MidiConfig (..)
  , MidiPort (..)
  , OutHandle
  , OutPort
  , cloneOutPort
  , freeOutHandle
  , freeOutPort
  , newOutHandle
  , outSendMsg1
  )
import Libremidi.Common
  ( Err
  , ErrM
  , UniquePtr
  , aliveUniquePtr
  , consumeUniquePtr
  , newUniquePtr
  , runErrM
  , unRunErrM
  , withUniquePtr'
  )
import Libremidi.Foreign qualified as LMF
import Libremidi.Simple (findOutPort)
import Minipat.Live.Attrs (IsAttrs (..), attrsSingleton)
import Minipat.Midi.Count (CountM, throwErrM)
import Nanotime (PosixTime, TimeDelta, threadDelayDelta)
import Prettyprinter (Pretty (..))

isNoteOff :: LiveMsg -> Bool
isNoteOff = \case
  LiveMsgChan _ (ChanDataVoice cvd) ->
    case cvd of
      ChanVoiceDataNoteOn _ 0 -> True
      ChanVoiceDataNoteOff _ _ -> True
      _ -> False
  _ -> False

mkNoteOff :: Channel -> ChanData -> Maybe LiveMsg
mkNoteOff c = \case
  ChanDataVoice (ChanVoiceDataNoteOn n v)
    | v > 0 ->
        Just (LiveMsgChan c (ChanDataVoice (ChanVoiceDataNoteOn n 0)))
  _ -> Nothing

-- | We order so that note offs come before other messages
newtype SortedMsg = SortedMsg {unSortedMsg :: LiveMsg}
  deriving stock (Show)
  deriving newtype (Eq)

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

data SetDefault = SetDefaultNo | SetDefaultYes
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Default SetDefault where
  def = SetDefaultNo

newtype PortName = PortName {unPortName :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString, Pretty)

data PortSel
  = PortSelDefault
  | PortSelName !PortName
  | PortSelPrefix !Text
  deriving stock (Eq, Ord, Show)

psToText :: PortSel -> Text
psToText = \case
  PortSelDefault -> "!"
  PortSelName (PortName t) -> t
  PortSelPrefix t -> T.snoc t '*'

psFromText :: Text -> PortSel
psFromText t =
  if t == "!"
    then PortSelDefault
    else
      let mzc = T.unsnoc t
      in  case mzc of
            Just (z, c) ->
              if c == '*'
                then PortSelPrefix z
                else PortSelName (PortName t)
            Nothing -> PortSelName (PortName t)

instance Pretty PortSel where
  pretty = pretty . psToText

instance IsAttrs PortSel where
  toAttrs = attrsSingleton "port" . DatumString . psToText

instance Default PortSel where
  def = PortSelDefault

instance IsString PortSel where
  fromString = psFromText . T.pack

data PortMsg = PortMsg
  { pmPort :: !PortSel
  , pmMsg :: !LiveMsg
  }
  deriving stock (Eq, Ord, Show)

data OutState = OutState
  { osPort :: !OutPort
  , osHandle :: !(UniquePtr LMF.OutHandle)
  , osHeap :: !(TVar (Heap TimedMsg))
  }
  deriving stock (Eq)

newOutState :: OutPort -> OutHandle -> IO OutState
newOutState op oh = OutState op <$> newUniquePtr oh <*> newTVarIO Heap.empty

freeOutState :: OutState -> IO ()
freeOutState (OutState port handUniq heapVar) = do
  atomically (writeTVar heapVar Heap.empty)
  consumeUniquePtr handUniq >>= freeOutHandle
  freeOutPort port

withOutHandle :: OutState -> (OutHandle -> ErrM ()) -> ErrM ()
withOutHandle (OutState _ handUniq heapVar) f = do
  lock <- liftIO $ atomically $ do
    waiting <- fmap (not . Heap.null) (readTVar heapVar)
    alive <- aliveUniquePtr handUniq
    pure (waiting && alive)
  when lock (unRunErrM (withUniquePtr' handUniq (runErrM . f)))

data MidiErr
  = MidiErrMissingOutPort !PortSel
  | MidiErrLibErr !(Maybe PortSel) !Err
  deriving stock (Eq, Ord, Show)

instance Exception MidiErr

data MidiState = MidiState
  { msOutMap :: !(TVar (Map PortName OutState))
  , msOutDefault :: !(TVar (Maybe PortName))
  }
  deriving stock (Eq)

selectOutState :: PortSel -> MidiState -> STM (Maybe (PortName, OutState))
selectOutState ps (MidiState omv odv) = do
  om <- readTVar omv
  xod <- readTVar odv
  pure $ case ps of
    PortSelDefault -> fmap (\od -> (od, om Map.! od)) xod
    PortSelName pn -> fmap (pn,) (Map.lookup pn om)
    PortSelPrefix t -> find (\(PortName s, _) -> T.isPrefixOf t (T.toLower s)) (Map.toList om)

insertOutState :: PortName -> OutState -> SetDefault -> MidiState -> IO ()
insertOutState pn os de (MidiState omv odv) = do
  mz <- atomically $ do
    when (de == SetDefaultYes) (writeTVar odv (Just pn))
    stateTVar omv (\m -> (Map.lookup pn m, Map.insert pn os m))
  for_ mz freeOutState

deleteOutState :: PortSel -> MidiState -> IO (Maybe PortName)
deleteOutState ps ms@(MidiState omv odv) = do
  mz <- atomically $ do
    mz <- selectOutState ps ms
    case mz of
      Just (pn, _) -> do
        om <- readTVar omv
        xod <- readTVar odv
        writeTVar omv (Map.delete pn om)
        case xod of
          Just od | od == pn -> writeTVar odv Nothing
          _ -> pure ()
      Nothing -> pure ()
    pure mz
  case mz of
    Just (pn, os) -> do
      freeOutState os
      pure (Just pn)
    Nothing -> pure Nothing

getOutDefault :: MidiState -> IO (Maybe PortName)
getOutDefault ms = readTVarIO (msOutDefault ms)

setOutDefault :: PortName -> MidiState -> IO ()
setOutDefault pn ms = atomically (writeTVar (msOutDefault ms) (Just pn))

data MidiEnv = MidiEnv
  { meApi :: !Api
  , meState :: !MidiState
  }
  deriving stock (Eq)

type MidiM = CountM MidiErr MidiEnv

errM :: (Err -> MidiErr) -> ErrM a -> MidiM (Maybe a)
errM f m = do
  ea <- liftIO (runErrM m)
  either (\e -> Nothing <$ throwErrM (f e)) (pure . Just) ea

errM_ :: (Err -> MidiErr) -> ErrM () -> MidiM ()
errM_ f m = do
  ea <- liftIO (runErrM m)
  either (throwErrM . f) pure ea

openOutPort' :: PortName -> OutPort -> SetDefault -> (MidiConfig -> IO MidiConfig) -> MidiM ()
openOutPort' pn op de f = do
  MidiEnv api ms <- ask
  mop' <- errM (MidiErrLibErr (Just (PortSelName pn))) (cloneOutPort op)
  case mop' of
    Nothing -> pure ()
    Just op' -> do
      c <- liftIO $ do
        x <- newUniquePtr op'
        f (def {mcPort = Just (MidiPortOut x)})
      moh <- errM (MidiErrLibErr (Just (PortSelName pn))) (newOutHandle api c)
      case moh of
        Nothing -> pure ()
        Just oh -> liftIO $ do
          os <- newOutState op oh
          insertOutState pn os de ms

selectOutPort :: PortSel -> IO (Maybe (PortName, OutPort))
selectOutPort ps =
  let f = fmap (fmap (first PortName)) . findOutPort
  in  case ps of
        PortSelDefault -> pure Nothing
        PortSelName (PortName t) -> f (t ==)
        PortSelPrefix t -> let t' = T.toLower t in f (T.isPrefixOf t' . T.toLower)

openOutPort :: PortSel -> SetDefault -> (MidiConfig -> IO MidiConfig) -> MidiM ()
openOutPort ps de f = do
  mx <- liftIO (selectOutPort ps)
  case mx of
    Nothing -> throwErrM (MidiErrMissingOutPort ps)
    Just (pn, op) -> openOutPort' pn op de f

closeOutPort :: PortSel -> MidiM ()
closeOutPort ps = do
  ms <- asks meState
  mx <- liftIO (deleteOutState ps ms)
  case mx of
    Nothing -> throwErrM (MidiErrMissingOutPort ps)
    Just _ -> pure ()

withOutPort :: PortSel -> (OutState -> ErrM ()) -> MidiM ()
withOutPort ps f = do
  ms <- asks meState
  mz <- liftIO (atomically (selectOutState ps ms))
  case mz of
    Nothing -> throwErrM (MidiErrMissingOutPort ps)
    Just (pn, os) -> errM_ (MidiErrLibErr (Just (PortSelName pn))) (f os)

sendLiveMsg :: VSM.IOVector Word8 -> OutHandle -> LiveMsg -> ErrM ()
sendLiveMsg buf oh lm = unRunErrM $ do
  len <- fmap fromIntegral (mutEncode lm buf)
  -- coercion is safe: Word8 -> CUChar
  VSM.unsafeWith buf (\ptr -> runErrM (outSendMsg1 oh (coerce ptr) len))

sendPortMsg' :: VSM.IOVector Word8 -> PortMsg -> MidiM ()
sendPortMsg' buf (PortMsg ps lm) =
  withOutPort ps (\os -> withOutHandle os (\oh -> sendLiveMsg buf oh lm))

sendPortMsgs :: (Foldable f) => Int -> Maybe TimeDelta -> f PortMsg -> MidiM ()
sendPortMsgs maxLen mayDelay msgs = do
  buf <- liftIO (VSM.new maxLen)
  for_ msgs $ \pm -> do
    sendPortMsg' buf pm
    liftIO (for_ mayDelay threadDelayDelta)
