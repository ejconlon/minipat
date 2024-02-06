{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Prelude where

-- TODO Explicit exports

import Control.Applicative (Alternative (..))
import Dahdit.Midi.Osc (Datum (..))
import Data.Char (isAlpha, isAlphaNum)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Looksee qualified as L
import Minipat.Ast (Ident (..), Pattern (..), Select (..))
import Minipat.Dirt.Attrs (Attr (..), Attrs, DatumProxy (..), IsAttrs (..))
import Minipat.Dirt.Notes
import Minipat.Eval (evalPat)
import Minipat.Parser (P, identP, selectP)
import Minipat.Stream (Stream (..), streamInnerBind)

-- Start with some private parsing stuff

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

-- General combinators

setIn, (#) :: (IsAttrs a, IsAttrs b) => Stream a -> Stream b -> Stream Attrs
setIn p1 p2 = streamInnerBind p1 $ \m1 ->
  let a1 = toAttrs m1 in fmap (\m2 -> toAttrs m2 <> a1) p2
(#) = setIn

pF :: (Pattern f, Real a) => Text -> f a -> f (Attr Float)
pF k = fmap (Attr k . realToFrac)

pI :: (Pattern f, Integral a) => Text -> f a -> f (Attr Int32)
pI k = fmap (Attr k . fromIntegral)

attrPat :: Pattern f => Text -> f a -> f (Attr a)
attrPat k = fmap (Attr k)

datumAttrPat :: Pattern f => DatumProxy a -> Text -> Text -> f (Attr a)
datumAttrPat dp k = attrPat k . datumPat dp

-- Specific combinators

data Sound = Sound
  { soundIdent :: !Ident
  , soundNote :: !(Maybe Note)
  }
  deriving stock (Eq, Ord, Show)

instance IsAttrs Sound where
  toAttrs (Sound so mn) = Map.insert "sound" (DatumString (unIdent so)) (maybe Map.empty toAttrs mn)

sound, s :: Pattern f => Text -> f Sound
sound = fmap conv . parsePat (selectP identP noteP)
 where
  conv (Select so mn) = Sound so mn
s = sound

note, n :: Pattern f => Text -> f Note
note = parsePat noteP
n = note

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
