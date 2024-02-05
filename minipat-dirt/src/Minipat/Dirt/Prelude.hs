{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Prelude where

import Data.Int (Int32)
import Data.Text (Text)
import Minipat.Dirt.Osc (Attr (..), Attrs, DatumProxy, IsAttrs (..))
import Minipat.Dirt.Parser (datumPat, notePat, soundPat)
import Minipat.Stream (Stream (..), streamInnerBind)

setIn, (#) :: (IsAttrs a, IsAttrs b) => Stream a -> Stream b -> Stream Attrs
setIn p1 p2 = streamInnerBind p1 $ \m1 ->
  let a1 = toAttrs m1 in fmap (\m2 -> toAttrs m2 <> a1) p2
(#) = setIn

pF :: (Real a) => Text -> Stream a -> Stream (Attr Float)
pF k = fmap (Attr k . realToFrac)

pI :: (Integral a) => Text -> Stream a -> Stream (Attr Int32)
pI k = fmap (Attr k . fromIntegral)

stream :: Text -> Stream a -> Stream (Attr a)
stream k = fmap (Attr k)

pat :: DatumProxy a -> Text -> Text -> Stream (Attr a)
pat dp k = stream k . datumPat dp

sound, s :: Text -> Stream Attrs
sound = soundPat
s = sound

note, n :: Text -> Stream (Attr Int32)
note = notePat
n = note

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
    :: (Real a) => Stream a -> Stream (Attr Float)
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
    :: (Real a) => Stream a -> Stream (Attr Float)
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
