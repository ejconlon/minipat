{-# LANGUAGE OverloadedStrings #-}

module Minipat.Live.Extra
  ( pI
  , pF
  , sound
  , s
  , note
  , n
  )
where

import Control.Applicative (Alternative (..))
import Dahdit.Midi.Osc (Datum (..))
import Data.Char (isAlpha, isAlphaNum)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Looksee qualified as L
import Minipat.Ast (Ident (..), Select (..))
import Minipat.EStream (EStream (..))
import Minipat.Eval (evalPat)
import Minipat.Live.Attrs (Attr (..), IsAttrs (..), attrsInsert)
import Minipat.Live.Combinators (S)
import Minipat.Live.Datum (DatumProxy (..))
import Minipat.Live.Notes (ChordName, Note (..), OctNote (..), Octave (..), convChordName, convNoteName, octToNote)
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

datumPat :: DatumProxy a -> Text -> S a
datumPat = parsePat . datumP

octNoteP :: P OctNote
octNoteP = do
  noteRaw <- L.takeWhile1P isAlpha
  case convNoteName noteRaw of
    Nothing -> fail ("Not note name: " ++ T.unpack noteRaw)
    Just nn -> do
      moct <- fmap (fmap (Octave . fromInteger)) (L.optP L.intP)
      pure (OctNote moct nn)

noteP :: P Note
noteP =
  fmap octToNote octNoteP <|> fmap (Note . fromInteger) L.intP

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

attrPat :: Text -> S a -> S (Attr a)
attrPat k = fmap (Attr k)

datumAttrPat :: DatumProxy a -> Text -> Text -> S (Attr a)
datumAttrPat dp k = attrPat k . datumPat dp

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

sound, s :: Text -> S Sound
sound = parsePat soundP
s = sound

-- * Note

note, n :: Text -> S Note
note = parsePat noteP
n = note

-- * Chord

data Chord = Chord
  { chordRoot :: !Note
  , chordName :: !ChordName
  }
  deriving stock (Eq, Ord, Show)

-- TODO
-- chord, c :: Text -> S Chord

-- * Arp

data Arp = ArpUp | ArpDown deriving stock (Eq, Ord, Show, Enum, Bounded)

arpMap :: Map Text Arp
arpMap = Map.fromList [("up", ArpUp), ("down", ArpDown)]

arpP :: P Arp
arpP = ordP arpMap (fmap unIdent identP)

arp :: Text -> S Arp
arp = parsePat arpP

-- TODO
-- strum :: S Arp -> S Chord -> S Note
-- strum arps chords = undefined
