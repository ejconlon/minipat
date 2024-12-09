module Minipat.Rns.Song where
import Control.Monad.Identity (Identity)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Minipat.Live.Attrs (Attrs)

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

-- data Song = Song
--   { songFn :: !String
--   } deriving stock (Eq, Ord, Show)

-- ---@field file_name string
-- ---@field artist string
-- ---@field artist_observable renoise.Document.Observable
-- ---@field name string
-- ---@field name_observable renoise.Document.Observable
-- ---@field comments string[]
-- ---@field comments_observable renoise.Document.ObservableList
-- ---@field comments_assignment_observable renoise.Document.Observable
-- ---@field show_comments_after_loading boolean
-- ---@field show_comments_after_loading_observable renoise.Document.Observable
-- ---@field tool_data string?
-- ---@field rendering_progress number Range: (0.0 - 1.0)
-- ---@field transport renoise.Transport
-- ---@field sequencer renoise.PatternSequencer
-- ---@field pattern_iterator renoise.PatternIterator
-- ---@field sequencer_track_count integer
-- ---@field send_track_count integer
-- ---
-- ---@field instruments renoise.Instrument[]
-- ---@field instruments_observable renoise.Document.ObservableList
-- ---@field patterns renoise.Pattern[]
-- ---@field patterns_observable renoise.Document.ObservableList
-- ---@field tracks renoise.Track[]
-- ---@field tracks_observable renoise.Document.ObservableList
-- ---
-- ---@field selected_instrument renoise.Instrument
-- ---@field selected_instrument_observable renoise.Document.Observable
-- ---@field selected_instrument_index integer
-- ---@field selected_instrument_index_observable renoise.Document.Observable
-- ---
-- ---@field selected_phrase renoise.InstrumentPhrase?
-- ---@field selected_phrase_observable renoise.Document.Observable
-- ---@field selected_phrase_index integer
-- ---
-- ---@field selected_sample renoise.Sample?
-- ---@field selected_sample_observable renoise.Document.Observable
-- ---@field selected_sample_index integer
-- ---
-- ---@field selected_sample_modulation_set renoise.SampleModulationSet?
-- ---@field selected_sample_modulation_set_observable renoise.Document.Observable
-- ---@field selected_sample_modulation_set_index integer
--
-- ---@field selected_sample_device_chain renoise.SampleDeviceChain?
-- ---@field selected_sample_device_chain_observable renoise.Document.Observable
-- ---@field selected_sample_device_chain_index integer
-- ---
-- ---@field selected_sample_device renoise.AudioDevice?
-- ---@field selected_sample_device_observable renoise.Document.Observable
-- ---@field selected_sample_device_index integer
-- ---
-- ---@field selected_track renoise.Track
-- ---@field selected_track_observable renoise.Document.Observable
-- ---@field selected_track_index integer
-- ---@field selected_track_index_observable renoise.Document.Observable
-- ---
-- ---@field selected_track_device renoise.AudioDevice?
-- ---@field selected_track_device_observable renoise.Document.Observable
-- ---@field selected_track_device_index integer
-- ---
-- ---@field selected_device renoise.AudioDevice?
-- ---@field selected_device_observable renoise.Document.Observable
-- ---@field selected_device_index integer
-- ---
-- ---@field selected_parameter renoise.DeviceParameter?
-- ---@field selected_parameter_observable renoise.Document.Observable
-- ---
-- ---@field selected_automation_parameter renoise.DeviceParameter?
-- ---@field selected_automation_parameter_observable renoise.Document.Observable
-- ---@field selected_automation_device renoise.AudioDevice?
-- ---@field selected_automation_device_observable renoise.Document.Observable
-- ---
-- ---@field selected_pattern renoise.Pattern
-- ---@field selected_pattern_observable renoise.Document.Observable
-- ---@field selected_pattern_index integer
-- ---@field selected_pattern_index_observable renoise.Document.Observable
-- ---
-- ---@field selected_pattern_track renoise.PatternTrack
-- ---@field selected_pattern_track_observable renoise.Document.Observable
-- ---
-- ---@field selected_sequence_index integer
-- ---@field selected_sequence_index_observable renoise.Document.Observable
-- ---
-- ---@field selected_line renoise.PatternLine
-- ---@field selected_line_index integer
-- ---
-- ---@field selected_note_column renoise.NoteColumn?
-- ---@field selected_note_column_index integer
-- ---@field selected_effect_column renoise.EffectColumn?
-- ---@field selected_effect_column_index integer
-- ---@field selected_sub_column_type renoise.Song.SubColumnType

type M = Identity

songCanUndo :: M Bool
songCanUndo = undefined

songUndo :: M ()
songUndo = undefined

songCanRedo :: M Bool
songCanRedo = undefined

songRedo :: M ()
songRedo = undefined

data SongFn :: Type -> Type where
  SFCanUndo :: SongFn Bool
  SFUndo :: SongFn ()
  SFCanRedo :: SongFn Bool
  SFRedo :: SongFn ()
  SFDescribeUndo :: String -> SongFn ()
  SFInsertTrackAt :: Int -> SongFn ()
  SFDeleteTrackAt :: Int -> SongFn ()
  SFSwapTracksAt :: Int -> Int -> SongFn ()
  SFSelectPrevTrack :: SongFn ()
  SFSelectNextTrack :: SongFn ()
  SFInsertInstrumentAt :: Int -> SongFn InstId

deriving stock instance (Eq (SongFn a))

-- songFnReq :: SongFn a -> Attrs
-- songFnReq = undefined
--
-- songFnRes :: SongFn a -> Attrs -> Maybe a
-- songFnRes = undefined

-- ---Insert a new instrument at the given index. This will remap all existing
-- ---notes in all patterns, if needed, and also update all other instrument links
-- ---in the song. Can't have more than MAX_NUMBER_OF_INSTRUMENTS in a song.
-- ---@param index integer
-- ---@return renoise.Instrument
-- function renoise.Song:insert_instrument_at(index) end
--
-- ---Delete an existing instrument at the given index. Renoise needs at least one
-- ---instrument, thus trying to completely remove all instruments is not allowed.
-- ---This will remap all existing notes in all patterns and update all other
-- ---instrument links in the song.
-- ---@param index integer
-- function renoise.Song:delete_instrument_at(index) end
--
-- ---Swap the position of two instruments. Will remap all existing notes in all
-- ---patterns and update all other instrument links in the song.
-- ---@param index1 integer
-- ---@param index2 integer
-- function renoise.Song:swap_instruments_at(index1, index2) end
--
-- ---Access to a single instrument by index. Use properties 'instruments' to iterate
-- ---over all instruments and to query the instrument count.
-- ---@param index integer
-- ---@return renoise.Instrument
-- function renoise.Song:instrument(index) end
--
-- ---Captures the current instrument (selects the instrument) from the current
-- ---note column at the current cursor pos. Changes the selected instrument
-- ---accordingly, but does not return the result. When no instrument is present at
-- ---the current cursor pos, nothing will be done.
-- function renoise.Song:capture_instrument_from_pattern() end
--
-- ---Tries to captures the nearest instrument from the current pattern track,
-- ---starting to look at the cursor pos, then advancing until an instrument is
-- ---found. Changes the selected instrument accordingly, but does not return
-- ---the result. When no instruments (notes) are present in the current pattern
-- ---track, nothing will be done.
-- function renoise.Song:capture_nearest_instrument_from_pattern() end
--
-- ---Access to a single pattern by index. Use properties 'patterns' to iterate
-- ---over all patterns and to query the pattern count.
-- ---@param index integer
-- ---@return renoise.Pattern
-- function renoise.Song:pattern(index) end
--
-- ---When rendering (see rendering, renoise.song().rendering_progress),
-- ---the current render process is canceled. Otherwise, nothing is done.
-- function renoise.Song:cancel_rendering() end
--
-- ---@class RenderOptions
-- ---by default the song start.
-- ---@field start_pos renoise.SongPos?
-- ---by default the song end.
-- ---@field end_pos renoise.SongPos?
-- ---by default the players current rate.
-- ---@field sample_rate (22050|44100|48000|88200|96000|192000)?
-- ---by default 32.
-- ---@field bit_depth (16|24|32)?
-- ---by default "default".
-- ---@field interpolation ("default"|"precise")?
-- ---by default "high".
-- ---@field priority ("low"|"realtime"|"high")?
--
-- ---Start rendering a section of the song or the whole song to a WAV file.
-- ---
-- ---Rendering job will be done in the background and the call will return
-- ---back immediately, but the Renoise GUI will be blocked during rendering. The
-- ---passed `rendering_done_callback` function is called as soon as rendering is
-- ---done, e.g. successfully completed.
-- ---
-- ---While rendering, the rendering status can be polled with the `song().rendering`
-- ---and `song().rendering_progress` properties, for example, in idle notifier
-- ---loops. If starting the rendering process fails (because of file IO errors for
-- ---example), the render function will return false and the error message is set
-- ---as the second return value. On success, only a single `true` value is
-- ---returned.
-- ---
-- ---To render only specific tracks or columns, mute the undesired tracks/columns
-- ---before starting to render.
-- ---
-- ---Parameter `file_name` must point to a valid, maybe already existing file. If it
-- ---already exists, the file will be silently overwritten. The renderer will
-- ---automatically add a ".wav" extension to the file_name, if missing.
-- ---
-- ---Parameter `rendering_done_callback` is ONLY called when rendering has succeeded.
-- ---You can do something with the file you've passed to the renderer here, like
-- ---for example loading the file into a sample buffer.
-- ---@param options RenderOptions
-- ---@param filename string
-- ---@param rendering_done_callback fun()
-- ---@return boolean success, string error?
-- ---@overload fun(self, filename: string, rendering_done_callback: fun()): boolean, string?
-- function renoise.Song:render(options, filename, rendering_done_callback) end
--
-- ---Load all global MIDI mappings in the song into a XRNM file.
-- ---Returns true when loading/saving succeeded, else false and the error message.
-- ---@param filename string
-- ---@return boolean success, string error?
-- function renoise.Song:load_midi_mappings(filename) end
--
-- ---Save all global MIDI mappings in the song into a XRNM file.
-- ---Returns true when loading/saving succeeded, else false and the error message.
-- ---@param filename string
-- ---@return boolean success, string error?
-- function renoise.Song:save_midi_mappings(filename) end
--
-- ---clear all MIDI mappings in the song
-- function renoise.Song:clear_midi_mappings() end
--
