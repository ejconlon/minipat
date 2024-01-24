module Minipat.Dirt.OSC where

-- Useful aliases for Superdirt params from sardine
paramAliases :: [(String, String)]
paramAliases =
  [ ("lpf", "cutoff")
  , ("lpq", "resonance")
  , ("hpf", "hcutoff")
  , ("lpq", "resonance")
  , ("bpf", "bandf")
  , ("bpq", "resonance")
  , ("res", "resonance")
  , ("midi", "midinote")
  , ("n", "midinote")
  , ("oct", "octave")
  , ("accel", "accelerate")
  , ("leg", "legato")
  , ("delayt", "delaytime")
  , ("delayfb", "delayfeedback")
  , ("phasr", "phaserrate")
  , ("phasd", "phaserdepth")
  , ("tremrate", "tremolorate")
  , ("tremd", "tremolodepth")
  , ("dist", "distort")
  , ("o", "orbit")
  , ("ts", "timescale")
  ]
