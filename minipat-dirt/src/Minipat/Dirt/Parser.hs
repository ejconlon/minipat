{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Parser
  ( datumPat
  , notePat
  , soundPat
  )
where

import Control.Applicative (Alternative (..))
import Dahdit.Midi.Osc (Datum (..))
import Data.Char (isAlpha, isAlphaNum)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text qualified as T
import Looksee qualified as L
import Minipat.Ast (Ident (..), Pattern (..), Select (..))
import Minipat.Dirt.Notes
import Minipat.Dirt.Osc (Attr (..), Attrs, DatumProxy (..), attrs)
import Minipat.Eval (evalPat)
import Minipat.Parser (P, identP, selectP)

datumP :: DatumProxy a -> P a
datumP = \case
  DatumProxyInt32 -> fmap fromInteger L.intP
  DatumProxyInt64 -> fmap fromInteger L.intP
  DatumProxyFloat -> fmap realToFrac L.sciP
  DatumProxyDouble -> fmap realToFrac L.sciP
  DatumProxyString -> fmap unIdent identP

-- TODO figure out out to propagate error
parsePat :: (Pattern f) => P a -> Text -> f a
parsePat p = either (pure patEmpty) id . evalPat p

datumPat :: (Pattern f) => DatumProxy a -> Text -> f a
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

-- TODO IsAttrs instance for Note instead
notePat :: (Pattern f) => Text -> f (Attr Int32)
notePat = fmap conv . parsePat noteP
 where
  conv = Attr "note" . fromIntegral . unNote

-- TODO IsAttrs instance for sound
soundPat :: (Pattern f) => Text -> f Attrs
soundPat = fmap conv . parsePat (selectP identP L.uintP)
 where
  conv (Select a ms) = attrs (("sound", DatumString (unIdent a)) : maybe [] (\n -> [("note", DatumInt32 (fromInteger n))]) ms)
