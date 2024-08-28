{-# LANGUAGE OverloadedStrings #-}

module Minipat.Midi.Midi where

import Control.Monad.IO.Class (liftIO)
import Dahdit.Midi.Osc (Datum (..))
import Minipat.Live.Attrs (IsAttrs (..), attrsSingleton)
import Prettyprinter (Pretty)
import Data.Heap (Heap)
import Data.Heap qualified as Heap
import Data.String (IsString)
import Control.Exception (Exception)
import Dahdit.Midi.Midi (ChanData (..), ChanVoiceData (..), Channel, LiveMsg (..))
import Data.Map.Strict (Map)
import Data.Text (Text)
import Nanotime (PosixTime, TimeDelta)
import Foreign.ForeignPtr (ForeignPtr)
import Libremidi.Api (OutPort, OutHandle)
import Libremidi.Common (ErrM, Err, runErrM)
import Minipat.Midi.Count (CountM, countErrM, runCountM)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO, writeTVar)

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

newtype PortName = PortName { unPortName :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString, Pretty)

instance IsAttrs PortName where
  toAttrs (PortName x) = attrsSingleton "port" (DatumString x)

data PortMsg = PortMsg
  { pmPort :: !(Maybe PortName)
  , pmMsg :: !LiveMsg
  } deriving stock (Eq, Ord, Show)

data OutState = OutState
  { osPort :: !OutPort
  , osHandle :: !OutHandle
  , osHeap :: !(TVar (Heap TimedMsg))
  } deriving stock (Eq)

newOutState :: OutPort -> OutHandle -> IO OutState
newOutState op oh = OutState op oh <$> newTVarIO Heap.empty

data MidiState = MidiState
  { msOutMap :: !(TVar (Map PortName OutState))
  , msOutDefault :: !(TVar (Maybe PortName))
  } deriving stock (Eq)

data OpenErr =
    OpenErrMissingOutPort !PortName
  | OpenErrLibErr !(Maybe PortName) !Err
  deriving stock (Eq, Ord, Show)

instance Exception OpenErr

data SendErr =
    SendErrNoOutPort
  | SendErrMissingOutPort !PortName
  | SendErrLibErr !PortName !Err
  deriving stock (Eq, Ord, Show)

instance Exception SendErr

data MidiErr =
    MidiErrOpen !OpenErr
  | MidiErrSend !SendErr
  deriving stock (Eq, Ord, Show)

instance Exception MidiErr

type MidiM = CountM MidiErr MidiState

-- addOutPort :: PortName -> Bool -> PortState -> IO ()
-- addOutPort pn de ps = do
--
-- closeOutPort :: PortName -> PortState -> IO ()
-- closeOutPort = undefined
--
-- setOutDefault :: Maybe PortName -> PortState -> IO ()
-- setOutDefault mpn ps = atomically (writeTVar (psOutDefault ps) mpn)

errM :: (Err -> MidiErr) -> ErrM a -> MidiM (Maybe a)
errM f m = do
  ea <- liftIO (runErrM m)
  case ea of
    Left e -> countErrM (f e) >> pure Nothing
    Right a -> pure (Just a)

errM_ :: (Err -> MidiErr) -> ErrM () -> MidiM ()
errM_ f m = do
  ea <- liftIO (runErrM m)
  case ea of
    Left e -> countErrM (f e)
    Right _ -> pure ()

-- addOutState :: OutPort -> OutHandle -> Bool -> MidiM ()
-- addOutState op oh de ms = do
--   os <- newOutState op oh
--   n <- withForeignPtr op outPortName
--   atomically $ do
--     modifyTVar (msOutMap ms) $ \m -> Map.insert n
--

openOutPort :: OutPort -> Bool -> MidiM ()
openOutPort fop de = do
  name <- undefined
  undefined

closeOutPort :: OutPort -> MidiM ()
closeOutPort = error "TODO"

openNamedOutPort :: PortName -> Bool -> MidiM ()
openNamedOutPort = error "TODO"

closeNamedOutPort :: PortName -> MidiM ()
closeNamedOutPort = error "TODO"

sendPortMsgs :: (Foldable f) => Int -> Maybe TimeDelta -> f PortMsg -> MidiM ()
sendPortMsgs = error "TODO"

-- sendLiveMsgs maxLen mayDelay outSt msgs = do
--   buf <- liftIO (VSM.new maxLen)
--   let send m = do
--         len <- fmap fromIntegral (mutEncode m buf)
--         VSM.unsafeWith buf (\ptr -> R.sendUnsafeMessage device ptr len)
--   for_ msgs $ \m -> do
--     send m
--     for_ mayDelay threadDelayDelta

