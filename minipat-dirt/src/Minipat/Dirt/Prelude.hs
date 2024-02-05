{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Prelude where

import Dahdit.Midi.Osc (Datum (..), DatumType (..), IsDatum (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
-- import Minipat.Dirt.Eval (liveEvalNotePat, liveEvalPat, liveEvalSoundPat)
import Minipat.Dirt.Osc (Attrs)
import Minipat.Stream (Stream (..), streamInnerBind)

setIn, (#) :: Stream Attrs -> Stream Attrs -> Stream Attrs
setIn p1 p2 = streamInnerBind p1 (\m1 -> fmap (<> m1) p2)
(#) = setIn

pF :: (Real a) => Text -> Stream a -> Stream Attrs
pF k = fmap (Map.singleton k . DatumFloat . realToFrac)

pI :: (Integral a) => Text -> Stream a -> Stream Attrs
pI k = fmap (Map.singleton k . DatumInt32 . fromIntegral)

-- pat :: DatumType -> Text -> Text -> Stream Attrs
-- pat dt k t = stream k (liveEvalPat dt t)

stream :: (IsDatum a) => Text -> Stream a -> Stream Attrs
stream k = fmap (Map.singleton k . toDatum)

-- sound, s :: Text -> Stream Attrs
-- sound = liveEvalSoundPat
-- s = sound
--
-- note, n :: Text -> Stream Attrs
-- note = liveEvalNotePat
-- n = note

-- TODO essentially - midinote = note . fmap (- 60)
-- default note is c5, so we subtract 60 to get to note 0
-- midinote :: Text -> Stream Attrs

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
    :: (Real a) => Stream a -> Stream Attrs
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
    :: (Real a) => Stream a -> Stream Attrs
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
