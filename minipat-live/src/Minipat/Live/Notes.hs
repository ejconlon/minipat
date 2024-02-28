{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Minipat.Live.Notes where

import Dahdit.Midi.Osc (Datum (..))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Text (Text)
import Minipat.Live.Attrs (IsAttrs (..), attrsSingleton)
import Prettyprinter (Pretty (..))

data NoteName
  = NoteNameCF
  | NoteNameC
  | NoteNameCS
  | NoteNameDF
  | NoteNameD
  | NoteNameDS
  | NoteNameEF
  | NoteNameE
  | NoteNameES
  | NoteNameFF
  | NoteNameF
  | NoteNameFS
  | NoteNameGF
  | NoteNameG
  | NoteNameGS
  | NoteNameAF
  | NoteNameA
  | NoteNameAS
  | NoteNameBF
  | NoteNameB
  | NoteNameBS
  deriving stock (Eq, Ord, Enum, Bounded, Show)

instance Pretty NoteName where
  pretty = \case
    NoteNameCF -> "cf"
    NoteNameC -> "c"
    NoteNameCS -> "cs"
    NoteNameDF -> "df"
    NoteNameD -> "d"
    NoteNameDS -> "ds"
    NoteNameEF -> "ef"
    NoteNameE -> "e"
    NoteNameES -> "es"
    NoteNameFF -> "ff"
    NoteNameF -> "f"
    NoteNameFS -> "fs"
    NoteNameGF -> "gf"
    NoteNameG -> "g"
    NoteNameGS -> "gs"
    NoteNameAF -> "af"
    NoteNameA -> "a"
    NoteNameAS -> "as"
    NoteNameBF -> "bf"
    NoteNameB -> "b"
    NoteNameBS -> "bs"

noteValue :: NoteName -> Integer
noteValue = \case
  NoteNameCF -> -1
  NoteNameC -> 0
  NoteNameCS -> 1
  NoteNameDF -> 1
  NoteNameD -> 2
  NoteNameDS -> 3
  NoteNameEF -> 3
  NoteNameE -> 4
  NoteNameES -> 5
  NoteNameFF -> 4
  NoteNameF -> 5
  NoteNameFS -> 6
  NoteNameGF -> 6
  NoteNameG -> 7
  NoteNameGS -> 8
  NoteNameAF -> 8
  NoteNameA -> 9
  NoteNameAS -> 10
  NoteNameBF -> 10
  NoteNameB -> 11
  NoteNameBS -> 12

newtype Octave = Octave {unOctave :: Integer}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Pretty)

-- Default octave is -1, making C default to MIDI note 0
defaultOctave :: Octave
defaultOctave = Octave (-1)

-- | A note split into octave and name
data OctNote = OctNote
  { onOctave :: !(Maybe Octave)
  , onName :: !NoteName
  }
  deriving stock (Eq, Ord, Show)

instance Pretty OctNote where
  pretty (OctNote moct nn) = pretty nn <> pretty (fromMaybe defaultOctave moct)

octNoteIsMidi :: OctNote -> Bool
octNoteIsMidi (OctNote moct nn) =
  case moct of
    Nothing -> True
    Just (Octave oct) ->
      if
        | oct == -1 -> nn == NoteNameC
        | oct == 9 -> nn <= NoteNameG
        | otherwise -> oct >= 0 && oct <= 8

-- TODO Change to LinNote, add DirtNote/MidiNote newtypes, and remove IsAttrs instance

-- | An integral note type that can represent notes outside the MIDI scale.
-- This is rooted at C5, MIDI note 60, so care must be taken to adjust before
-- converting to/from MIDI values.
newtype Note = Note {unNote :: Integer}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Pretty)

instance IsAttrs Note where
  toAttrs (Note n) = attrsSingleton "note" (DatumInt32 (fromInteger n))

c5MidiNum :: Integer
c5MidiNum = 72

noteToMidi :: Note -> Integer
noteToMidi = (c5MidiNum +) . unNote

midiToNote :: Integer -> Note
midiToNote = Note . subtract c5MidiNum

noteFreq :: (Floating a) => Note -> a
noteFreq n =
  let m = noteToMidi n
  in  440 * (2 ** ((fromInteger m - 69) / 12))

-- Midi notes are between 0 (C-1) and 127 (G9)
-- Piano notes are between 21 (A0) and 108 (C8)
noteIsMidi :: Note -> Bool
noteIsMidi n = let m = noteToMidi n in m >= 0 && m < 128

noteAddInterval :: Interval -> Note -> Note
noteAddInterval (Interval i) (Note n) = Note (i + n)

noteToOct :: Note -> OctNote
noteToOct (Note n) = OctNote (Just (Octave (div n 12 - 1))) (toEnum (mod (fromInteger n) 12))

linSubInterval :: Note -> Note -> Interval
linSubInterval (Note a) (Note b) = Interval (a - b)

octToNote :: OctNote -> Note
octToNote (OctNote moct nn) =
  let oct = maybe 5 unOctave moct
  in  Note ((oct + 1) * 12 + noteValue nn - c5MidiNum)

newtype Interval = Interval {unInterval :: Integer}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

octAddInterval :: Interval -> OctNote -> OctNote
octAddInterval i = noteToOct . noteAddInterval i . octToNote

convNoteName :: Text -> Maybe NoteName
convNoteName = \case
  "cf" -> Just NoteNameCF
  "cflat" -> Just NoteNameCF
  "c" -> Just NoteNameC
  "cs" -> Just NoteNameCS
  "csharp" -> Just NoteNameCS
  "df" -> Just NoteNameDF
  "dflat" -> Just NoteNameDF
  "d" -> Just NoteNameD
  "ds" -> Just NoteNameDS
  "dsharp" -> Just NoteNameDS
  "ef" -> Just NoteNameEF
  "eflat" -> Just NoteNameEF
  "e" -> Just NoteNameE
  "es" -> Just NoteNameES
  "esharp" -> Just NoteNameES
  "ff" -> Just NoteNameFF
  "fflat" -> Just NoteNameFF
  "f" -> Just NoteNameF
  "fs" -> Just NoteNameFS
  "fsharp" -> Just NoteNameFS
  "gf" -> Just NoteNameGF
  "gflat" -> Just NoteNameGF
  "g" -> Just NoteNameG
  "gs" -> Just NoteNameGS
  "gsharp" -> Just NoteNameGS
  "af" -> Just NoteNameAF
  "aflat" -> Just NoteNameAF
  "a" -> Just NoteNameA
  "as" -> Just NoteNameAS
  "asharp" -> Just NoteNameAS
  "bf" -> Just NoteNameBF
  "bflat" -> Just NoteNameBF
  "b" -> Just NoteNameB
  "bs" -> Just NoteNameBS
  "bsharp" -> Just NoteNameBS
  _ -> Nothing

data ChordName
  = ChordNameMaj
  | ChordNameAug
  | ChordName6
  | ChordName69
  | ChordNameMaj7
  | ChordNameMaj9
  | ChordNameAdd9
  | ChordNameMaj11
  | ChordNameAdd11
  | ChordNameMaj13
  | ChordNameAdd13
  | ChordNameDom7
  | ChordNameDom9
  | ChordNameDom11
  | ChordNameDom13
  | ChordName7Flat5
  | ChordName7Sharp5
  | ChordName7Flat9
  | ChordName9
  | ChordName11
  | ChordName13
  | ChordNameMin
  | ChordNameDim
  | ChordNameMinSharp5
  | ChordNameMin6
  | ChordNameMin69
  | ChordNameMin7Flat5
  | ChordNameMin7
  | ChordNameMin7Sharp5
  | ChordNameMin7Flat9
  | ChordNameMin7Sharp9
  | ChordNameDim7
  | ChordNameMin9
  | ChordNameMin11
  | ChordNameMin13
  | ChordNameMinMaj7
  | ChordName1
  | ChordName5
  | ChordNameSus2
  | ChordNameSus4
  | ChordName7Sus2
  | ChordName7Sus4
  | ChordName9Sus4
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Pretty ChordName where
  pretty = \case
    ChordNameMaj -> "maj"
    ChordNameAug -> "aug"
    ChordName6 -> "6"
    ChordName69 -> "69"
    ChordNameMaj7 -> "maj7"
    ChordNameMaj9 -> "maj9"
    ChordNameAdd9 -> "add9"
    ChordNameMaj11 -> "maj11"
    ChordNameAdd11 -> "add11"
    ChordNameMaj13 -> "maj13"
    ChordNameAdd13 -> "add13"
    ChordNameDom7 -> "dom7"
    ChordNameDom9 -> "dom9"
    ChordNameDom11 -> "dom11"
    ChordNameDom13 -> "dom13"
    ChordName7Flat5 -> "7f5"
    ChordName7Sharp5 -> "7s5"
    ChordName7Flat9 -> "7f9"
    ChordName9 -> "9"
    ChordName11 -> "11"
    ChordName13 -> "13"
    ChordNameMin -> "min"
    ChordNameDim -> "dim"
    ChordNameMinSharp5 -> "s5"
    ChordNameMin6 -> "min6"
    ChordNameMin69 -> "min69"
    ChordNameMin7Flat5 -> "min7f5"
    ChordNameMin7 -> "min7"
    ChordNameMin7Sharp5 -> "min7s5"
    ChordNameMin7Flat9 -> "min7f9"
    ChordNameMin7Sharp9 -> "min7s9"
    ChordNameDim7 -> "dim7"
    ChordNameMin9 -> "min9"
    ChordNameMin11 -> "min11"
    ChordNameMin13 -> "min13"
    ChordNameMinMaj7 -> "mmaj7"
    ChordName1 -> "1"
    ChordName5 -> "5"
    ChordNameSus2 -> "sus2"
    ChordNameSus4 -> "sus4"
    ChordName7Sus2 -> "7sus2"
    ChordName7Sus4 -> "7sus4"
    ChordName9Sus4 -> "9sus4"

-- Chord mapping a la Tidal
convChordName :: Text -> Maybe ChordName
convChordName = \case
  "major" -> Just ChordNameMaj
  "maj" -> Just ChordNameMaj
  "M" -> Just ChordNameMaj
  "aug" -> Just ChordNameAug
  "plus" -> Just ChordNameAug
  "sharp5" -> Just ChordNameAug
  "six" -> Just ChordName6
  "6" -> Just ChordName6
  "69" -> Just ChordName69
  "sixNine" -> Just ChordName69
  "six9" -> Just ChordName69
  "sixby9" -> Just ChordName69
  "6by9" -> Just ChordName69
  "major7" -> Just ChordNameMaj7
  "maj7" -> Just ChordNameMaj7
  "M7" -> Just ChordNameMaj7
  "major9" -> Just ChordNameMaj9
  "maj9" -> Just ChordNameMaj9
  "M9" -> Just ChordNameMaj9
  "add9" -> Just ChordNameAdd9
  "major11" -> Just ChordNameMaj11
  "maj11" -> Just ChordNameMaj11
  "M11" -> Just ChordNameMaj11
  "add11" -> Just ChordNameAdd11
  "major13" -> Just ChordNameMaj13
  "maj13" -> Just ChordNameMaj13
  "M13" -> Just ChordNameMaj13
  "add13" -> Just ChordNameAdd13
  "dom7" -> Just ChordNameDom7
  "dom9" -> Just ChordNameDom9
  "dom11" -> Just ChordNameDom11
  "dom13" -> Just ChordNameDom13
  "sevenFlat5" -> Just ChordName7Flat5
  "7f5" -> Just ChordName7Flat5
  "sevenSharp5" -> Just ChordName7Sharp5
  "7s5" -> Just ChordName7Sharp5
  "sevenFlat9" -> Just ChordName7Flat9
  "7f9" -> Just ChordName7Flat9
  "nine" -> Just ChordName9
  "eleven" -> Just ChordName11
  "11" -> Just ChordName11
  "thirteen" -> Just ChordName13
  "13" -> Just ChordName13
  "minor" -> Just ChordNameMin
  "min" -> Just ChordNameMin
  "m" -> Just ChordNameMin
  "diminished" -> Just ChordNameDim
  "dim" -> Just ChordNameDim
  "minorSharp5" -> Just ChordNameMinSharp5
  "msharp5" -> Just ChordNameMinSharp5
  "mS5" -> Just ChordNameMinSharp5
  "minor6" -> Just ChordNameMin6
  "min6" -> Just ChordNameMin6
  "m6" -> Just ChordNameMin6
  "minorSixNine" -> Just ChordNameMin69
  "minor69" -> Just ChordNameMin69
  "min69" -> Just ChordNameMin69
  "minSixNine" -> Just ChordNameMin69
  "m69" -> Just ChordNameMin69
  "mSixNine" -> Just ChordNameMin69
  "m6by9" -> Just ChordNameMin69
  "minor7flat5" -> Just ChordNameMin7Flat5
  "minor7f5" -> Just ChordNameMin7Flat5
  "min7flat5" -> Just ChordNameMin7Flat5
  "min7f5" -> Just ChordNameMin7Flat5
  "m7flat5" -> Just ChordNameMin7Flat5
  "m7f5" -> Just ChordNameMin7Flat5
  "minor7" -> Just ChordNameMin7
  "min7" -> Just ChordNameMin7
  "m7" -> Just ChordNameMin7
  "minor7sharp5" -> Just ChordNameMin7Sharp5
  "minor7s5" -> Just ChordNameMin7Sharp5
  "min7sharp5" -> Just ChordNameMin7Sharp5
  "min7s5" -> Just ChordNameMin7Sharp5
  "m7sharp5" -> Just ChordNameMin7Sharp5
  "m7s5" -> Just ChordNameMin7Sharp5
  "minor7flat9" -> Just ChordNameMin7Flat9
  "minor7f9" -> Just ChordNameMin7Flat9
  "min7flat9" -> Just ChordNameMin7Flat9
  "min7f9" -> Just ChordNameMin7Flat9
  "m7flat9" -> Just ChordNameMin7Flat9
  "m7f9" -> Just ChordNameMin7Flat9
  "minor7sharp9" -> Just ChordNameMin7Sharp9
  "minor7s9" -> Just ChordNameMin7Sharp9
  "min7sharp9" -> Just ChordNameMin7Sharp9
  "min7s9" -> Just ChordNameMin7Sharp9
  "m7sharp9" -> Just ChordNameMin7Sharp9
  "m7s9" -> Just ChordNameMin7Sharp9
  "diminished7" -> Just ChordNameDim7
  "dim7" -> Just ChordNameDim7
  "minor9" -> Just ChordNameMin9
  "min9" -> Just ChordNameMin9
  "m9" -> Just ChordNameMin9
  "minor11" -> Just ChordNameMin11
  "min11" -> Just ChordNameMin11
  "m11" -> Just ChordNameMin11
  "minor13" -> Just ChordNameMin13
  "min13" -> Just ChordNameMin13
  "m13" -> Just ChordNameMin13
  "minorMajor7" -> Just ChordNameMinMaj7
  "minMaj7" -> Just ChordNameMinMaj7
  "mmaj7" -> Just ChordNameMinMaj7
  "one" -> Just ChordName1
  "1" -> Just ChordName1
  "five" -> Just ChordName5
  "5" -> Just ChordName5
  "sus2" -> Just ChordNameSus2
  "sus4" -> Just ChordNameSus4
  "sevenSus2" -> Just ChordName7Sus2
  "7sus2" -> Just ChordName7Sus2
  "sevenSus4" -> Just ChordName7Sus4
  "7sus4" -> Just ChordName7Sus4
  "nineSus4" -> Just ChordName9Sus4
  "ninesus4" -> Just ChordName9Sus4
  "9sus4" -> Just ChordName9Sus4
  _ -> Nothing

chordNotes :: ChordName -> Seq Int
chordNotes = \case
  ChordNameMaj -> [0, 4, 7]
  ChordNameAug -> [0, 4, 8]
  ChordName6 -> [0, 4, 7, 9]
  ChordName69 -> [0, 4, 7, 9, 14]
  ChordNameMaj7 -> [0, 4, 7, 11]
  ChordNameMaj9 -> [0, 4, 7, 11, 14]
  ChordNameAdd9 -> [0, 4, 7, 14]
  ChordNameMaj11 -> [0, 4, 7, 11, 14, 17]
  ChordNameAdd11 -> [0, 4, 7, 17]
  ChordNameMaj13 -> [0, 4, 7, 11, 14, 21]
  ChordNameAdd13 -> [0, 4, 7, 21]
  ChordNameDom7 -> [0, 4, 7, 10]
  ChordNameDom9 -> [0, 4, 7, 14]
  ChordNameDom11 -> [0, 4, 7, 17]
  ChordNameDom13 -> [0, 4, 7, 21]
  ChordName7Flat5 -> [0, 4, 6, 10]
  ChordName7Sharp5 -> [0, 4, 8, 10]
  ChordName7Flat9 -> [0, 4, 7, 10, 13]
  ChordName9 -> [0, 4, 7, 10, 14]
  ChordName11 -> [0, 4, 7, 10, 14, 17]
  ChordName13 -> [0, 4, 7, 10, 14, 17, 21]
  ChordNameMin -> [0, 3, 7]
  ChordNameDim -> [0, 3, 6]
  ChordNameMinSharp5 -> [0, 3, 8]
  ChordNameMin6 -> [0, 3, 7, 9]
  ChordNameMin69 -> [0, 3, 9, 7, 14]
  ChordNameMin7Flat5 -> [0, 3, 6, 10]
  ChordNameMin7 -> [0, 3, 7, 10]
  ChordNameMin7Sharp5 -> [0, 3, 8, 10]
  ChordNameMin7Flat9 -> [0, 3, 7, 10, 13]
  ChordNameMin7Sharp9 -> [0, 3, 7, 10, 14]
  ChordNameDim7 -> [0, 3, 6, 9]
  ChordNameMin9 -> [0, 3, 7, 10, 14]
  ChordNameMin11 -> [0, 3, 7, 10, 14, 17]
  ChordNameMin13 -> [0, 3, 7, 10, 14, 17, 21]
  ChordNameMinMaj7 -> [0, 3, 7, 11]
  ChordName1 -> [0]
  ChordName5 -> [0, 7]
  ChordNameSus2 -> [0, 2, 7]
  ChordNameSus4 -> [0, 5, 7]
  ChordName7Sus2 -> [0, 2, 7, 10]
  ChordName7Sus4 -> [0, 5, 7, 10]
  ChordName9Sus4 -> [0, 5, 7, 10, 14]
