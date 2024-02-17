{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Params where

import Data.Int (Int32)
import Data.Text (Text)
import Minipat.Dirt.Attrs (Attr (..))
import Minipat.EStream (EStream)

pF :: (Real a) => Text -> EStream a -> EStream (Attr Float)
pF k = fmap (Attr k . realToFrac)

pI :: (Integral a) => Text -> EStream a -> EStream (Attr Int32)
pI k = fmap (Attr k . fromIntegral)

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
    :: (Real a) => EStream a -> EStream (Attr Float)
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
    :: (Real a) => EStream a -> EStream (Attr Float)
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
