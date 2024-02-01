{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Notes where

import Data.Char (isAlpha)
import Data.Text (Text)
import Data.Text qualified as T
import Looksee qualified as L
import Minipat.Parser (P)
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

noteValue :: NoteName -> Int
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

newtype Octave = Octave {unOctave :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Pretty)

-- | A note split into octave and name
-- Default octave is -1, making C default to MIDI note 0
data OctNote = OctNote
  { onOctave :: !(Maybe Octave)
  , onName :: !NoteName
  }
  deriving stock (Eq, Ord, Show)

instance Pretty OctNote where
  pretty (OctNote moct nn) =
    let x = pretty nn
    in  maybe x ((x <>) . pretty) moct

octNoteIsMidi :: OctNote -> Bool
octNoteIsMidi (OctNote moct nn) =
  case moct of
    Nothing -> True
    Just (Octave oct) ->
      if
        | oct == -1 -> nn == NoteNameC
        | oct == 9 -> nn <= NoteNameG
        | otherwise -> oct >= 0 && oct <= 8

-- | An integral note type that can represent notes outside the MIDI scale.
newtype LinNote = LinNote {unLinNote :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- Midi notes are between 0 (C-1) and 127 (G9)
-- Piano notes are between 21 (A0) and 108 (C8)
linIsMidiNote :: LinNote -> Bool
linIsMidiNote (LinNote n) = n >= 0 && n < 127

linFreq :: LinNote -> Double
linFreq (LinNote n) = 440 * (2 ** ((fromIntegral n - 69) / 12))

newtype Interval = Interval {unInterval :: Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

linAddInterval :: Interval -> LinNote -> LinNote
linAddInterval (Interval i) (LinNote n) = LinNote (i + n)

linToOct :: LinNote -> OctNote
linToOct (LinNote n) = OctNote (Just (Octave (div n 12 - 1))) (toEnum (mod n 12))

linSubInterval :: LinNote -> LinNote -> Interval
linSubInterval (LinNote a) (LinNote b) = Interval (a - b)

octToLin :: OctNote -> LinNote
octToLin (OctNote moct nn) =
  let oct = maybe (-1) unOctave moct
  in  LinNote (((oct + 1) * 12) + noteValue nn)

octAddInterval :: Interval -> OctNote -> OctNote
octAddInterval i = linToOct . linAddInterval i . octToLin

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

parseOctNote :: P OctNote
parseOctNote = do
  noteRaw <- L.takeWhile1P isAlpha
  case convNoteName noteRaw of
    Nothing -> fail ("Not note name: " ++ T.unpack noteRaw)
    Just nn -> do
      moct <- fmap (fmap (Octave . fromInteger)) (L.optP L.intP)
      pure (OctNote moct nn)
