{-# LANGUAGE OverloadedStrings #-}

-- | Some extra combinators that are too specific for all backends, but
-- may be useful for more than one backend.
module Minipat.Live.Extra
  ( parsePat
  , parseDatum
  , pI
  , pF
  , Sound (..)
  , parseSound
  , Note (..)
  , parseNote
  , parseMidiNote
  , midiToNote
  , noteToMidi
  , Chord (..)
  , Arp (..)
  , parseArp
  )
where

import Control.Applicative (Alternative (..))
import Dahdit.Midi.Osc (Datum (..))
import Data.Char (isAlpha, isAlphaNum)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Looksee qualified as L
import Minipat.Ast (Ident (..), Select (..))
import Minipat.EStream (EStream (..))
import Minipat.Eval (evalPat)
import Minipat.Live.Attrs (Attr (..), IsAttrs (..), attrsInsert, attrsSingleton)
import Minipat.Live.Combinators (S)
import Minipat.Live.Datum (DatumProxy (..))
import Minipat.Live.Notes
  ( ChordName
  , LinNote (..)
  , OctNote (..)
  , Octave (..)
  , convChordName
  , convNoteName
  , linToOct
  , octToLin
  )
import Minipat.Parser (P, identP, selectP)
import Prettyprinter (Pretty (..))

datumP :: DatumProxy a -> P a
datumP = \case
  DatumProxyInt32 -> fmap fromInteger L.intP
  DatumProxyInt64 -> fmap fromInteger L.intP
  DatumProxyFloat -> fmap realToFrac L.sciP
  DatumProxyDouble -> fmap realToFrac L.sciP
  DatumProxyString -> fmap unIdent identP

parsePat :: P a -> Text -> S a
parsePat p = EStream . evalPat p

parseDatum :: DatumProxy a -> Text -> S a
parseDatum = parsePat . datumP

octNoteP :: Integer -> P OctNote
octNoteP defOct = do
  noteRaw <- L.takeWhile1P isAlpha
  case convNoteName noteRaw of
    Nothing -> fail ("Not note name: " ++ T.unpack noteRaw)
    Just nn -> do
      oct <- fmap (Octave . fromMaybe defOct) (L.optP L.intP)
      pure (OctNote oct nn)

chordNameP :: P ChordName
chordNameP = do
  nameRaw <- L.takeWhile1P isAlphaNum
  case convChordName nameRaw of
    Nothing -> fail ("Not chord name: " ++ T.unpack nameRaw)
    Just cn -> pure cn

ordP :: (Ord a, Show a) => Map a b -> P a -> P b
ordP m pa =
  pa >>= \a -> case Map.lookup a m of
    Nothing -> fail ("Not found: " ++ show a)
    Just b -> pure b

parseAttr :: Text -> S a -> S (Attr a)
parseAttr k = fmap (Attr k)

parseDatumAttr :: DatumProxy a -> Text -> Text -> S (Attr a)
parseDatumAttr dp k = parseAttr k . parseDatum dp

pF :: (Real a) => Text -> S a -> S (Attr Float)
pF k = fmap (Attr k . realToFrac)

pI :: (Integral a) => Text -> S a -> S (Attr Int32)
pI k = fmap (Attr k . fromIntegral)

-- * Sound

data Sound = Sound
  { soundIdent :: !Ident
  , soundNote :: !(Maybe Note)
  }
  deriving stock (Eq, Ord, Show)

instance Pretty Sound where
  pretty (Sound so mn) = pretty so <> maybe mempty ((":" <>) . pretty) mn

instance IsAttrs Sound where
  toAttrs (Sound so mn) = attrsInsert "sound" (DatumString (unIdent so)) (toAttrs mn)

soundP :: P Sound
soundP = fmap (\(Select so mn) -> Sound so mn) (selectP identP noteP)

parseSound :: Text -> S Sound
parseSound = parsePat soundP

-- * Note

-- | This is rooted at C5, MIDI note 60, so care must be taken to adjust before
-- converting to/from MIDI values.
newtype Note = Note {unNote :: Int32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Pretty)

instance IsAttrs Note where
  toAttrs (Note x) = attrsSingleton "note" (DatumInt32 x)

noteLinOffset :: Integer
noteLinOffset = 72

noteDefaultOctave :: Integer
noteDefaultOctave = 5

noteToLin :: Note -> LinNote
noteToLin = LinNote . (noteLinOffset +) . fromIntegral . unNote

linToNote :: LinNote -> Note
linToNote = Note . fromInteger . subtract noteLinOffset . unLinNote

noteToOct :: Note -> OctNote
noteToOct = linToOct . noteToLin

octToNote :: OctNote -> Note
octToNote = linToNote . octToLin

noteP :: P Note
noteP =
  fmap octToNote (octNoteP noteDefaultOctave)
    <|> fmap (Note . fromInteger) L.intP

parseNote :: Text -> S Note
parseNote = parsePat noteP

-- * MidiNote

midiLinOffset :: Int32
midiLinOffset = 0

midiNoteP :: P Note
midiNoteP =
  fmap octToNote (octNoteP noteDefaultOctave)
    <|> fmap (midiToNote . fromInteger) L.intP

midiToNote :: Int32 -> Note
midiToNote = Note . subtract midiLinOffset

noteToMidi :: Note -> Int32
noteToMidi = (midiLinOffset +) . unNote

parseMidiNote :: Text -> S Note
parseMidiNote = parsePat midiNoteP

-- * Note conversions

-- * Chord

data Chord n = Chord
  { chordRoot :: !n
  , chordName :: !ChordName
  }
  deriving stock (Eq, Ord, Show)

-- TODO implement
-- parseChord :: Text -> S Chord

-- * Arp

data Arp = ArpUp | ArpDown deriving stock (Eq, Ord, Show, Enum, Bounded)

arpMap :: Map Text Arp
arpMap = Map.fromList [("up", ArpUp), ("down", ArpDown)]

arpP :: P Arp
arpP = ordP arpMap (fmap unIdent identP)

parseArp :: Text -> S Arp
parseArp = parsePat arpP

-- TODO implement
-- strum :: S Arp -> S Chord -> S Note
-- strum arps chords = undefined
