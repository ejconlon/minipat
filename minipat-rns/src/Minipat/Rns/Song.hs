-- | Adapting https://github.com/renoise/definitions
module Minipat.Rns.Song where

import Control.Monad.Identity (Identity)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Minipat.Live.Attrs (Attrs)
import Dahdit.Midi.Osc (DatumType (..))
import Data.Int (Int32)

newtype TrackId = TrackId { unTrackId :: Int } deriving stock (Eq, Ord, Show)
newtype GroupId = GroupId { unGroupId :: Int } deriving stock (Eq, Ord, Show)
newtype InstId = InstId { unInstId :: Int } deriving stock (Eq, Ord, Show)

newtype PatIx = PatIx { unPatIx :: Int } deriving stock (Eq, Ord, Show)
newtype SeqIx = SeqIx { unSeqIx :: Int } deriving stock (Eq, Ord, Show)
newtype LineIx = LineIx { unLineIx :: Int } deriving stock (Eq, Ord, Show)
newtype TrackIx = TrackIx { unTrackIx :: Int } deriving stock (Eq, Ord, Show)
newtype ColIx = ColIx { unColIx :: Int } deriving stock (Eq, Ord, Show)

data SongPos = SongPos
  { spSeq :: !SeqIx
  , spLine :: !LineIx
  } deriving stock (Eq, Ord, Show)

data PatSelPos = PatSelPos
  { pssLine :: !LineIx
  , pssTrack :: !TrackIx
  , pssCol :: !ColIx
  } deriving stock (Eq, Ord, Show)

data PatSel = PatSel
  { pasStart :: !(Maybe PatSelPos)
  , pasEnd :: !(Maybe PatSelPos)
  } deriving stock (Eq, Ord, Show)

data PhraseSelPos = PhraseSelPos
  { pspLine :: !LineIx
  , pspCol :: !ColIx
  } deriving stock (Eq, Ord, Show)

data PhraseSel = PhraseSel
  { phsStart :: !(Maybe PhraseSelPos)
  , phsEnd :: !(Maybe PhraseSelPos)
  } deriving stock (Eq, Ord, Show)

maxNumberOfInstruments :: Int
maxNumberOfInstruments = 255

data SubCol =
    SubColNote
  | SubColInst
  | SubColVol
  | SubColPan
  | SubColDel
  | SubColSampEffNum
  | SubColSampEffAmt
  | SubColEffNum
  | SubColEffAmt
  deriving stock (Eq, Ord, Show, Enum, Bounded)

scFromInt :: Int -> SubCol
scFromInt = \case
  1 -> SubColNote
  2 -> SubColInst
  3 -> SubColVol
  4 -> SubColPan
  5 -> SubColDel
  6 -> SubColSampEffNum
  7 -> SubColSampEffAmt
  8 -> SubColEffNum
  9 -> SubColEffAmt
  _ -> undefined

data ApiType =
    ApiTypeClass !String
  | ApiTypeOpt !ApiType
  | ApiTypeArr !ApiType
  | ApiTypeBool
  | ApiTypeString
  | ApiTypeInt
  | ApiTypeFloat
  | ApiTypeUnit
  deriving stock (Eq, Ord, Show)

data RX = RO | RW
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Field = Field
  { fName :: !String
  , fRx :: !RX
  , fTy :: !ApiType
  } deriving stock (Eq, Ord, Show)

data Method = Method
  { mName :: !String
  , mArgTys :: ![ApiType]
  , mRetTy :: !ApiType
  } deriving stock (Eq, Ord, Show)

data Class = Class
  { cName :: !String
  , cFields :: ![Field]
  , cMethods :: ![Method]
  } deriving stock (Eq, Ord, Show)

-- Class "" [] []
-- Method "" [] []
-- Field "" []

tc :: String -> ApiType
tc = ApiTypeClass

to :: ApiType -> ApiType
to = ApiTypeOpt

ti :: ApiType
ti = ApiTypeInt

toi :: ApiType
toi = to ti

ts :: ApiType
ts = ApiTypeString

ta :: ApiType -> ApiType
ta = ApiTypeArr

tb :: ApiType
tb = ApiTypeBool

tf :: ApiType
tf = ApiTypeFloat

tu :: ApiType
tu = ApiTypeUnit

classes :: [Class]
classes =
  [ Class "renoise.SongPos"
    [ Field "sequence" RW ti
    , Field "line" RW ti
    ]
    [
    ]
  , Class "Song"
    [ Field "file_name" RO ts
    , Field "name" RW ts
    , Field "comments" RW (ta ts)
    , Field "show_comments_after_loading" RW tb
    , Field "tool_data" RW (to ts)
    , Field "rendering" RO tb
    , Field "rendering_progress" RO tf
    , Field "transport" RO (tc "Transport")
    , Field "sequencer" RO (tc "PatternSequencer")
    , Field "send_track_count" RO ti
    , Field "instruments" RO (ta (tc "Instrument"))
    , Field "patterns" RO (ta (tc "Pattern"))
    , Field "tracks" RO (ta (tc "Track"))
    -- , Field "selected_instrument" RO (tc "Instrument")
    -- , Field "selected_instrument_index" RO ti
    -- , Field "selected_phrase" RO (to (tc "InstrumentPhrase"))
    -- , Field "selected_phrase_index" RO ti
    -- , Field "selected_sample" RO (to (tc "Sample"))
    -- , Field "selected_sample_index" RO ti
    -- , Field "selected_sample_modulation_set" RO (to (tc "SampleModulationSet"))
    -- , Field "selected_sample_modulation_set_index" RO ti
    -- , Field "selected_sample_device_chain" RO (to (tc "SampleDeviceChain"))
    -- , Field "selected_sample_device_chain_index" RO ti
    -- , Field "selected_sample_device" RO (to (tc "AudioDevice"))
    -- , Field "selected_sample_device_index" RO ti
    -- , Field "selected_sample_track" RO (tc "Track")
    -- , Field "selected_sample_track_index" RO ti
    -- , Field "selected_sample_track_device" RO (to (tc "AudioDevice"))
    -- , Field "selected_sample_track_device_index" RO ti
    ]
    [ Method "can_undo" [] tb
    , Method "undo" [] tu
    , Method "can_redo" [] tb
    , Method "redo" [] tu
    , Method "describe_undo" [ts] tu
    , Method "insert_track_at" [ti] (tc "Track")
    , Method "delete_track_at" [ti] tu
    , Method "swap_tracks_at" [ti, ti] tu
    , Method "track" [ti] (tc "Track")
    ]
  , Class "PatternSelection"
    [ Field "start_line" RW toi
    , Field "start_track" RW toi
    , Field "start_column" RW toi
    , Field "end_line" RW toi
    , Field "end_track" RW toi
    , Field "end_column" RW toi
    ]
    [
    ]
  , Class "PhraseSelection"
    [ Field "start_line" RW toi
    , Field "start_column" RW toi
    , Field "end_line" RW toi
    , Field "end_column" RW toi
    ]
    [
    ]
  ]

