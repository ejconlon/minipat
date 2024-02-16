{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Prelude where

-- TODO Explicit exports

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
import Minipat.Classes (Flow (..), Pattern (..))
import Minipat.Dirt.Attrs (Attr (..), Attrs, DatumProxy (..), IsAttrs (..))
import Minipat.Dirt.Notes (ChordName, Note (..), OctNote (..), Octave (..), convChordName, convNoteName, octToNote)
import Minipat.Eval (PatternEval, evalPat)
import Minipat.Parser (P, identP, selectP)

-- Start with some private parsing stuff

datumP :: DatumProxy a -> P a
datumP = \case
  DatumProxyInt32 -> fmap fromInteger L.intP
  DatumProxyInt64 -> fmap fromInteger L.intP
  DatumProxyFloat -> fmap realToFrac L.sciP
  DatumProxyDouble -> fmap realToFrac L.sciP
  DatumProxyString -> fmap unIdent identP

-- TODO figure out out to propagate error
parsePat :: (PatternEval f) => P a -> Text -> f a
parsePat p = either (pure patEmpty) id . evalPat p

datumPat :: (PatternEval f) => DatumProxy a -> Text -> f a
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

setIn, (#) :: (Flow f, IsAttrs a, IsAttrs b) => f a -> f b -> f Attrs
setIn = flowInnerApply (\m1 m2 -> toAttrs m2 <> toAttrs m1)
(#) = setIn

pF :: (Pattern f, Real a) => Text -> f a -> f (Attr Float)
pF k = fmap (Attr k . realToFrac)

pI :: (Pattern f, Integral a) => Text -> f a -> f (Attr Int32)
pI k = fmap (Attr k . fromIntegral)

attrPat :: (Pattern f) => Text -> f a -> f (Attr a)
attrPat k = fmap (Attr k)

datumAttrPat :: (PatternEval f) => DatumProxy a -> Text -> Text -> f (Attr a)
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

sound, s :: (PatternEval f) => Text -> f Sound
sound = parsePat soundP
s = sound

note, n :: (PatternEval f) => Text -> f Note
note = parsePat noteP
n = note

data Chord = Chord
  { chordRoot :: !Note
  , chordName :: !ChordName
  }
  deriving stock (Eq, Ord, Show)

-- chord :: Pattern f =>

data Arp = ArpUp | ArpDown deriving stock (Eq, Ord, Show, Enum, Bounded)

arpMap :: Map Text Arp
arpMap = Map.fromList [("up", ArpUp), ("down", ArpDown)]

arpP :: P Arp
arpP = ordP arpMap (fmap unIdent identP)

arp :: (PatternEval f) => Text -> f Arp
arp = parsePat arpP

-- strum :: Stream Arp -> Stream Chord -> Stream Note
-- strum arps chords = undefined

-- Params

-- TODO check these are all float, not int
-- Basic effect parameters
accelerate
  , attack
  , bandf
  , bandq
  , cutoff
  , delay
  , delayfeedback
  , delaytime
  , distort
  , djf
  , dry
  , hcutoff
  , hold
  , hresonance
  , legato
  , leslie
  , lrate
  , lsize
  , pan
  , phaserdepth
  , phaserrate
  , release
  , resonance
  , room
  , size
  , slide
  , squiz
  , sustain
  , tremolodepth
  , tremolorate
    :: (Pattern f, Real a) => f a -> f (Attr Float)
accelerate = pF "accelerate"
attack = pF "attack"
bandf = pF "bandf"
bandq = pF "bandq"
cutoff = pF "cutoff"
delay = pF "delay"
delayfeedback = pF "delayfeedback"
delaytime = pF "delaytime"
distort = pF "distort"
djf = pF "djf"
dry = pF "dry"
hcutoff = pF "hcutoff"
hold = pF "hold"
hresonance = pF "hresonance"
legato = pF "legato"
leslie = pF "leslie"
lrate = pF "lrate"
lsize = pF "lsize"
pan = pF "pan"
phaserdepth = pF "phaserdepth"
phaserrate = pF "phaserrate"
release = pF "release"
resonance = pF "resonance"
room = pF "room"
size = pF "size"
slide = pF "slide"
squiz = pF "squiz"
sustain = pF "sustain"
tremolodepth = pF "tremolodepth"
tremolorate = pF "tremolorate"

-- Shorthand for those effect parameters
accel
  , att
  , bpf
  , bpq
  , delayfb
  , delayt
  , dist
  , hpf
  , hpq
  , leg
  , lpf
  , lpq
  , phasdp
  , phasr
  , rel
  , res
  , sz
  , tremdp
  , tremr
    :: (Pattern f, Real a) => f a -> f (Attr Float)
att = attack
bpf = bandf
bpq = bandq
delayfb = delayfeedback
delayt = delaytime
dist = distort
hpf = hcutoff
hpq = hresonance
leg = legato
lpf = cutoff
lpq = resonance
phasdp = phaserdepth
phasr = phaserrate
rel = release
res = resonance
sz = size
tremdp = tremolodepth
tremr = tremolorate
accel = accelerate

-- TODO add these?
-- , ("o", "orbit")
-- , ("midi", "midinote")
-- , ("ts", "timescale")
-- , ("n", "midinote")
-- , ("oct", "octave")
