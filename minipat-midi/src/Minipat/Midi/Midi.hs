{-# LANGUAGE OverloadedStrings #-}

module Minipat.Midi.Midi
  ( PortName (..)
  , PortSel (..)
  , PortMsg (..)
  , PortData (..)
  , SortedMsg (..)
  , TimedMsg (..)
  , SetDefault (..)
  , AutoConn (..)
  , MidiState (..)
  , MidiErr (..)
  , MidiEnv (..)
  , MidiM
  , openOutPort
  , newMidiState
  , sendPortMsg
  , sendPortMsgs
  , psFromText
  , mkNoteOff
  , setDefaultOutPort
  )
where

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, readTVarIO, stateTVar, writeTVar)
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
  , LogFun
  , LogLvl (..)
  , MidiConfig (..)
  , MidiPort (..)
  , OutHandle
  , OutPort
  , cloneOutPort
  , freeOutHandle
  , freeOutPort
  , newLogCb
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
import Libremidi.Simple qualified as LMS
import Minipat.Live.Attrs (ToAttrs (..), attrsSingleton)
import Minipat.Live.Logger (LogAction, logError, logWarn)
import Minipat.Midi.Count (CountM, throwErrM)
import Nanotime (PosixTime, TimeDelta, threadDelayDelta)
import Prettyprinter (Pretty (..))

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

instance ToAttrs PortSel where
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

isNoteOff :: PortMsg -> Bool
isNoteOff (PortMsg _ lm) = case lm of
  LiveMsgChan _ (ChanDataVoice cvd) ->
    case cvd of
      ChanVoiceDataNoteOn _ 0 -> True
      ChanVoiceDataNoteOff _ _ -> True
      _ -> False
  _ -> False

data PortData = PortData
  { pdPort :: !PortSel
  , pdChan :: !ChanData
  }
  deriving stock (Eq, Ord, Show)

mkNoteOff :: Channel -> PortData -> Maybe PortMsg
mkNoteOff c (PortData ps cd) = case cd of
  ChanDataVoice (ChanVoiceDataNoteOn n v)
    | v > 0 ->
        Just (PortMsg ps (LiveMsgChan c (ChanDataVoice (ChanVoiceDataNoteOn n 0))))
  _ -> Nothing

-- | We order so that note offs come before other messages
newtype SortedMsg = SortedMsg {unSortedMsg :: PortMsg}
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

data AutoConn = AutoConnNo | AutoConnYes
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Default AutoConn where
  def = AutoConnYes

data OutState = OutState
  { osPort :: !OutPort
  , osHandle :: !(UniquePtr LMF.OutHandle)
  }
  deriving stock (Eq)

newOutState :: OutPort -> OutHandle -> IO OutState
newOutState op oh = OutState op <$> newUniquePtr oh

freeOutState :: OutState -> IO ()
freeOutState (OutState port handUniq) = do
  consumeUniquePtr handUniq >>= freeOutHandle
  freeOutPort port

withOutHandle :: OutState -> (OutHandle -> ErrM ()) -> ErrM ()
withOutHandle (OutState _ handUniq) f = do
  alive <- liftIO (atomically (aliveUniquePtr handUniq))
  when alive (unRunErrM (withUniquePtr' handUniq (runErrM . f)))

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

newMidiState :: IO MidiState
newMidiState = MidiState <$> newTVarIO Map.empty <*> newTVarIO Nothing

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
    modifyTVar' odv $ \od -> case od of
      Just _ -> case de of
        SetDefaultNo -> od
        SetDefaultYes -> Just pn
      Nothing -> Just pn
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
  , meLogger :: !LogAction
  , meState :: !MidiState
  }

type MidiM = CountM MidiErr MidiEnv

errM :: (Err -> MidiErr) -> ErrM a -> MidiM (Maybe a)
errM f m = do
  ea <- liftIO (runErrM m)
  either (\e -> Nothing <$ throwErrM (f e)) (pure . Just) ea

errM_ :: (Err -> MidiErr) -> ErrM () -> MidiM ()
errM_ f m = do
  ea <- liftIO (runErrM m)
  either (throwErrM . f) pure ea

logFun :: LogAction -> LogFun
logFun logger = \case
  LogLvlWarn -> logWarn logger
  LogLvlErr -> logError logger

embedM :: LMS.MidiM a -> MidiM a
embedM m = do
  logger <- asks meLogger
  let fun = logFun logger
  liftIO (LMS.runMidiM m fun)

withMidiConfig :: PortName -> OutPort -> (MidiConfig -> MidiM ()) -> MidiM ()
withMidiConfig pn op f = do
  mop' <- errM (MidiErrLibErr (Just (PortSelName pn))) (cloneOutPort op)
  case mop' of
    Nothing -> pure ()
    Just op' -> do
      logger <- asks meLogger
      port <- liftIO (fmap MidiPortOut (newUniquePtr op'))
      warnCb <- liftIO (newLogCb (logFun logger) LogLvlWarn)
      errCb <- liftIO (newLogCb (logFun logger) LogLvlErr)
      f $
        def
          { mcPort = Just port
          , mcOnWarn = Just warnCb
          , mcOnErr = Just errCb
          }

openOutPort' :: PortName -> OutPort -> SetDefault -> MidiM ()
openOutPort' pn op de = withMidiConfig pn op $ \mc -> do
  MidiEnv api _ ms <- ask
  moh <- errM (MidiErrLibErr (Just (PortSelName pn))) (newOutHandle api mc)
  case moh of
    Nothing -> pure ()
    Just oh -> liftIO $ do
      os <- newOutState op oh
      insertOutState pn os de ms

selectOutPort :: PortSel -> MidiM (Maybe (PortName, OutPort))
selectOutPort ps =
  let f = fmap (fmap (first PortName)) . embedM . LMS.findOutPort
  in  case ps of
        PortSelDefault -> pure Nothing
        PortSelName (PortName t) -> f (t ==)
        PortSelPrefix t -> let t' = T.toLower t in f (T.isPrefixOf t' . T.toLower)

openOutPort :: PortSel -> SetDefault -> MidiM ()
openOutPort ps de = do
  mx <- selectOutPort ps
  case mx of
    Nothing -> throwErrM (MidiErrMissingOutPort ps)
    Just (pn, op) -> openOutPort' pn op de

closeOutPort :: PortSel -> MidiM ()
closeOutPort ps = do
  ms <- asks meState
  mx <- liftIO (deleteOutState ps ms)
  case mx of
    Nothing -> throwErrM (MidiErrMissingOutPort ps)
    Just _ -> pure ()

setDefaultOutPort :: PortSel -> MidiM ()
setDefaultOutPort ps = do
  ms <- asks meState
  mz <- liftIO (atomically (selectOutState ps ms))
  case mz of
    Nothing -> throwErrM (MidiErrMissingOutPort ps)
    Just (pn, _) -> liftIO (setOutDefault pn ms)

withOutPort :: PortSel -> AutoConn -> (OutState -> ErrM ()) -> MidiM ()
withOutPort ps _ac f = do
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

sendPortMsg :: VSM.IOVector Word8 -> AutoConn -> PortMsg -> MidiM ()
sendPortMsg buf ac (PortMsg ps lm) =
  withOutPort ps ac $ \os ->
    withOutHandle os $ \oh ->
      sendLiveMsg buf oh lm

sendPortMsgs :: (Foldable f) => Int -> Maybe TimeDelta -> AutoConn -> f PortMsg -> MidiM ()
sendPortMsgs maxLen mayDelay ac msgs = do
  buf <- liftIO (VSM.new maxLen)
  for_ msgs $ \pm -> do
    sendPortMsg buf ac pm
    liftIO (for_ mayDelay threadDelayDelta)
