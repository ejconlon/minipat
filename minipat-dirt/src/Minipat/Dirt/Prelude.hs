{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Prelude
  ( S
  , setIn
  , (#)
  , note
  , n
  , sound
  , s
  , arp
  , fast
  , slow
  , fastBy
  , slowBy
  , lateBy
  , earlyBy
  , layout
  , fastCat
  , slowCat
  , fastList
  , slowList
  , fastAppend
  , slowAppend
  )
where

import Control.Applicative (Alternative (..))
import Dahdit.Midi.Osc (Datum (..))
import Data.Char (isAlpha, isAlphaNum)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Looksee qualified as L
import Minipat.Ast (Ident (..), Select (..))
import Minipat.Classes (Flow (..))
import Minipat.Dirt.Attrs (Attr (..), Attrs, DatumProxy (..), IsAttrs (..))
import Minipat.Dirt.Notes (ChordName, Note (..), OctNote (..), Octave (..), convChordName, convNoteName, octToNote)
import Minipat.EStream
import Minipat.Eval (evalPat)
import Minipat.Parser (P, identP, selectP)
import Minipat.Time (CycleDelta, CycleTime)

type S = EStream

-- Start with some private parsing stuff

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

-- General combinators

setIn, (#) :: (IsAttrs a, IsAttrs b) => S a -> S b -> S Attrs
setIn = flowInnerApply (\m1 m2 -> toAttrs m2 <> toAttrs m1)
(#) = setIn

attrPat :: Text -> S a -> S (Attr a)
attrPat k = fmap (Attr k)

datumAttrPat :: DatumProxy a -> Text -> Text -> S (Attr a)
datumAttrPat dp k = attrPat k . datumPat dp

-- Specific combinators

data Sound = Sound
  { soundIdent :: !Ident
  , soundNote :: !(Maybe Note)
  }
  deriving stock (Eq, Ord, Show)

instance IsAttrs Sound where
  toAttrs (Sound so mn) = Map.insert "sound" (DatumString (unIdent so)) (maybe Map.empty toAttrs mn)

soundP :: P Sound
soundP = fmap (\(Select so mn) -> Sound so mn) (selectP identP noteP)

sound, s :: Text -> S Sound
sound = parsePat soundP
s = sound

note, n :: Text -> S Note
note = parsePat noteP
n = note

data Chord = Chord
  { chordRoot :: !Note
  , chordName :: !ChordName
  }
  deriving stock (Eq, Ord, Show)

-- TODO
-- chord, c :: Text -> S Chord

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

-- Shorthands

fast, slow :: S Rational -> S a -> S a
fast = estreamFast
slow = estreamSlow

fastBy, slowBy :: Rational -> S a -> S a
fastBy = estreamFastBy
slowBy = estreamSlowBy

lateBy, earlyBy :: CycleDelta -> S a -> S a
lateBy = estreamLateBy
earlyBy = estreamEarlyBy

layout :: Seq (CycleTime, CycleTime, S a) -> S a
layout = estreamPar . fmap (\(start, end, stream) -> estreamPieces mempty [(start, stream), (end, mempty)])

fastCat :: Seq (S a) -> S a
fastCat = estreamSeq . fmap (,1)

slowCat :: Seq (S a) -> S a
slowCat ss = slowBy (fromIntegral (Seq.length ss)) (fastCat ss)

fastList :: Seq a -> S a
fastList = fastCat . fmap pure

slowList :: Seq a -> S a
slowList as = slowBy (fromIntegral (Seq.length as)) (fastList as)

fastAppend :: S a -> S a -> S a
fastAppend s1 s2 = estreamSeq [(s1, 1), (s2, 1)]

slowAppend :: S a -> S a -> S a
slowAppend s1 s2 = slowBy 2 (fastAppend s1 s2)

-- TODO
-- seqPLoop :: Seq (CycleTime, CycleTime, S a) -> S a
-- rev :: S a -> S a
-- swingBy :: Rational -> S a -> S a
-- swing :: S Rational -> S a -> S a
-- echo
-- off
-- timeCat
-- randCat
-- wrandCat
-- wedge
