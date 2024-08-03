{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Midi message makers for SC-88 and the like
module Minipat.Midi.SC where

import Dahdit.Midi.Midi (ChanData (..), ChanVoiceData (..), LiveMsg (..))
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T

setControl :: Int -> Int -> Int -> Seq LiveMsg
setControl chan control value =
  Seq.singleton
    ( LiveMsgChan
        (fromIntegral chan)
        ( ChanDataVoice
            ( ChanVoiceControlChange
                (fromIntegral control)
                (fromIntegral value)
            )
        )
    )

setProgram :: Int -> Int -> Seq LiveMsg
setProgram chan program =
  Seq.singleton
    ( LiveMsgChan
        (fromIntegral chan)
        ( ChanDataVoice
            ( ChanVoiceProgramChange
                (fromIntegral program)
            )
        )
    )

setSound :: Int -> Int -> Int -> Seq LiveMsg
setSound chan inst var =
  mconcat
    [ setControl chan 0 var
    , setControl chan 32 0
    , setProgram chan inst
    ]

setLevel :: Int -> Int -> Seq LiveMsg
setLevel = flip setControl 7

setPan :: Int -> Int -> Seq LiveMsg
setPan = flip setControl 10

setReverb :: Int -> Int -> Seq LiveMsg
setReverb = flip setControl 91

setChorus :: Int -> Int -> Seq LiveMsg
setChorus = flip setControl 93

allSoundsOff :: Int -> Seq LiveMsg
allSoundsOff chan = setControl chan 120 0

-- Turn off non-sustained notes
allNotesOff :: Int -> Seq LiveMsg
allNotesOff chan = setControl chan 123 0

reinit :: Int -> Seq LiveMsg
reinit chan =
  mconcat
    [ setSound chan 0 0
    , setLevel chan 100
    , setPan chan 64
    , setReverb chan 40
    , setChorus chan 0
    ]

data Inst = Inst
  { instIx :: !Int
  , instProg :: !Int
  , instVar :: !Int
  , instName :: !Text
  }
  deriving stock (Eq, Ord, Show)

mkInst :: Int -> (Int, Int, Text) -> Inst
mkInst i (p, v, n) = Inst i (p - 1) v n

processName :: Text -> Text
processName = T.map $ \case
  '-' -> ' '
  '_' -> ' '
  c -> toLower c

matchName :: Text -> Text -> Bool
matchName t' n =
  let n' = processName n
  in  not (T.null (snd (T.breakOn t' n')))

findThing :: (x -> Text) -> (Int -> x -> y) -> Seq x -> Text -> Maybe y
findThing xName mkY xs t =
  let t' = processName t
  in  fmap
        (\i -> mkY i (Seq.index xs i))
        ( Seq.findIndexL
            (matchName t' . xName)
            xs
        )

findInstrument :: Text -> Maybe Inst
findInstrument = findThing (\(_, _, n) -> n) mkInst instruments

firstInstrument :: Inst
firstInstrument = mkInst 0 (Seq.index instruments 0)

nextInstrument :: Inst -> Inst
nextInstrument (Inst i _ _ _) =
  if i >= Seq.length instruments
    then mkInst 0 (Seq.index instruments 0)
    else let j = i + 1 in mkInst j (Seq.index instruments j)

setSoundNamed :: Int -> Text -> Seq LiveMsg
setSoundNamed chan frag =
  case findInstrument frag of
    Just (Inst _ prog var _) -> setSound chan (prog - 1) var
    Nothing -> error ("Could not find instrument: " <> T.unpack frag)

instruments :: Seq (Int, Int, Text)
instruments =
  [ (1, 0, "Piano 1")
  , (1, 8, "Piano 1w")
  , (1, 16, "Piano 1d")
  , (2, 0, "Piano 2")
  , (2, 8, "Piano 2w")
  , (3, 0, "Piano 3")
  , (3, 1, "EG+Rhodes1")
  , (3, 2, "EG+Rhodes2")
  , (3, 8, "Piano 3w")
  , (4, 0, "Honky-tonk")
  , (4, 8, "Old Upright")
  , (5, 0, "E.Piano 1")
  , (5, 8, "St.Soft EP")
  , (5, 16, "FM+SA EP")
  , (5, 24, "60's E.Piano")
  , (5, 25, "Hard Rhodes")
  , (5, 26, "MellowRhodes")
  , (6, 0, "E.Piano 2")
  , (6, 8, "Detuned EP 2")
  , (6, 16, "St.FM EP")
  , (6, 24, "Hard FM EP")
  , (7, 0, "Harpsichord")
  , (7, 8, "Coupled Hps.")
  , (7, 16, "Harpsi.w")
  , (7, 24, "Harpsi.o")
  , (8, 0, "Clav.")
  , (9, 0, "Celesta")
  , (10, 0, "Glockenspiel")
  , (11, 0, "Music Box")
  , (12, 0, "Vibraphone")
  , (12, 1, "Hard Vibe")
  , (12, 8, "Vib.w")
  , (13, 0, "Marimba")
  , (13, 8, "Marimba w")
  , (13, 16, "Barafon")
  , (13, 17, "Barafon 2")
  , (13, 24, "Log drum")
  , (14, 0, "Xylophone")
  , (15, 0, "Tublar-bell")
  , (15, 8, "Church Bell")
  , (15, 9, "Carillon")
  , (16, 0, "Santur")
  , (16, 1, "Santur 2")
  , (16, 8, "Cambalon")
  , (17, 0, "Organ 1")
  , (17, 1, "Organ 101")
  , (17, 8, "Detuned Or.1")
  , (17, 9, "Organ 109")
  , (17, 16, "60's Organ 1")
  , (17, 17, "60's Organ 2")
  , (17, 18, "60's Organ 3")
  , (17, 24, "Cheese Organ 1")
  , (17, 32, "Organ 4")
  , (17, 33, "Even Bar")
  , (17, 40, "Organ Bass")
  , (18, 0, "Organ 2")
  , (18, 1, "Organ 201")
  , (18, 8, "Detuned Or.2")
  , (18, 32, "Organ 5")
  , (19, 0, "Organ 3")
  , (19, 8, "Rotary Org.")
  , (19, 16, "Rotary Org.S")
  , (19, 24, "Rotary Org.F")
  , (20, 0, "Church Org.1")
  , (20, 8, "Church Org.2")
  , (20, 16, "Church Org.3")
  , (20, 24, "Organ Flute")
  , (20, 32, "Term.Flute")
  , (21, 0, "Reed Organ")
  , (22, 0, " Accordion Fr")
  , (22, 8, "Accordion It")
  , (23, 0, "Harmonica")
  , (23, 1, "Harmonica 2")
  , (24, 0, "Bandoneon")
  , (25, 0, "Nylon-str.Gt")
  , (25, 8, "Ukulele")
  , (25, 16, "Nylon Gt.o")
  , (25, 24, "Velo Harmnix")
  , (25, 32, "Nylon Gt.2")
  , (25, 40, "Lequint Gt.")
  , (26, 0, "Steel-str.Gt")
  , (26, 8, "12-str.Gt")
  , (26, 9, "Nylon+Steel")
  , (26, 16, "Mandolin")
  , (26, 32, "Steel Gt.2")
  , (27, 0, "Jazz Gt.")
  , (27, 1, "Mellow Gt.")
  , (27, 8, "Pedal Steel")
  , (28, 0, "Clean Gt.")
  , (28, 8, "Chorus Gt.")
  , (29, 0, "Muted Gt.")
  , (29, 1, "Muted Dis.Gt")
  , (29, 8, "Funk Pop")
  , (29, 16, "Funk Gt.2")
  , (30, 0, "Overdrive Gt")
  , (31, 0, "DistortionGt")
  , (31, 1, "Dist. Gt2")
  , (31, 2, "Dazed Guitar")
  , (31, 8, "Feedback Gt.")
  , (31, 9, "Feedback Gt2")
  , (31, 16, "Power Guitar")
  , (31, 17, "Power Gt.2")
  , (31, 18, "5th Dist.")
  , (31, 24, "Rock Rhythm")
  , (31, 25, "Rock Rhythm2")
  , (32, 0, "Gt.Harmonics")
  , (32, 8, "Gt.Feedback")
  , (32, 16, "Ac.Gt.Harmnx")
  , (33, 0, "Acoustic Bs.")
  , (34, 0, "Fingerd Bs.")
  , (34, 1, "Fingerd Bs2")
  , (34, 2, "Jazz Bass")
  , (35, 0, "Picked Bass")
  , (35, 8, "Muted PickBs.")
  , (36, 0, "Fletless Bs.")
  , (36, 1, "Fletless Bs2")
  , (36, 2, "Fletless Bs3")
  , (36, 3, "Fletless Bs4")
  , (36, 4, "Syn Fletless")
  , (36, 5, "Mr.Smooth")
  , (37, 0, "Slap Bass 1")
  , (37, 8, "Reso Slap")
  , (38, 0, "Slap Bass 2")
  , (39, 0, "Synth Bass 1")
  , (39, 1, "SynthBass101")
  , (39, 8, "Acid Bass")
  , (39, 9, "TB303 Bass")
  , (39, 10, "Tenko Bass")
  , (39, 16, "Reso SH Bass")
  , (40, 0, "Synth Bass 2")
  , (40, 1, "SynthBass201")
  , (40, 2, "Modular Bass")
  , (40, 3, "Seq Bass")
  , (40, 8, "Beef FM Bass")
  , (40, 9, "X Wire Bass")
  , (40, 16, "Rubber Bass")
  , (40, 17, "SH101 Bass 1")
  , (40, 18, "SH101 Bass 2")
  , (40, 19, "Smooth Bass")
  , (41, 0, "Violin")
  , (41, 8, "Slow Violin")
  , (42, 0, "Viola")
  , (43, 0, "Cello")
  , (44, 0, "Contrabass")
  , (45, 0, "Tremolo Str")
  , (45, 8, "Slow Tremolo")
  , (45, 9, "Suspense Str")
  , (46, 0, "PizzicatoStr")
  , (47, 0, "Harp")
  , (48, 0, "Timpani")
  , (49, 0, "Strings")
  , (49, 1, "Strings 2")
  , (49, 8, "Orchestra")
  , (49, 9, "Orchestra 2")
  , (49, 10, "Tremolo Orch")
  , (49, 11, "Choir Str.")
  , (49, 16, "St.Strings")
  , (49, 24, "Velo Strings")
  , (50, 0, "Slow Strings")
  , (50, 1, "SlowStrings2")
  , (50, 8, "Legato Str.")
  , (50, 9, "Warm Strings")
  , (50, 10, "St.Slow Str.")
  , (51, 0, "Syn.Strings1")
  , (51, 1, "OB Strings")
  , (51, 8, "Syn.Strings3")
  , (52, 0, "Syn.Strings2")
  , (53, 0, "Choir Aahs")
  , (53, 8, "St.Choir")
  , (53, 9, "Mello Choir")
  , (53, 32, "Choir Aahs 2")
  , (54, 0, "Voice Oohs")
  , (55, 0, "SynVox")
  , (55, 8, "Syn.Voice")
  , (56, 0, "OrchestraHit")
  , (56, 8, "Impact Hit")
  , (56, 9, "Philly Hit")
  , (56, 10, "Double Hit")
  , (56, 16, "Lo Fi Rave")
  , (57, 0, "Trumpet")
  , (57, 1, "Trumpet 2")
  , (57, 8, "Flugel Horn")
  , (57, 24, "Bright Tp.")
  , (57, 25, "Warm Tp.")
  , (58, 0, "Trombone")
  , (58, 1, "Trombone 2")
  , (59, 0, "Tuba")
  , (59, 1, "Tuba 2")
  , (60, 0, "MutedTrumpet")
  , (61, 0, "French Horns")
  , (61, 1, "Fr.Horn 2")
  , (61, 8, "Fr.Horn Solo")
  , (61, 16, "Horn Orch")
  , (62, 0, "Brass 1")
  , (62, 8, "Brass 2")
  , (62, 16, "Brass Fall")
  , (63, 0, "Synth Brass1")
  , (63, 1, "Poly Brass")
  , (63, 8, "Synth Brass3")
  , (63, 9, "Quack Brass")
  , (63, 16, "Octave Brass")
  , (64, 0, "Synth Brass2")
  , (64, 1, "Soft Brass")
  , (64, 8, "Synth Brass4")
  , (64, 16, "Velo Brass 1")
  , (64, 17, "Velo Brass 2")
  , (65, 0, "Soprano Sax")
  , (66, 0, "Alto Sax")
  , (66, 8, "Hyper Alto")
  , (67, 0, "Tenor Sax")
  , (67, 8, "BreathyTenor")
  , (68, 0, "Baritone Sax")
  , (69, 0, "Oboe")
  , (70, 0, "English Horn")
  , (71, 0, "Bassoon")
  , (72, 0, "Clarinet")
  , (72, 8, "Bs Clarinet")
  , (73, 0, "Piccolo")
  , (74, 0, "Flute")
  , (75, 0, "Recorder")
  , (76, 0, "Pan Flute")
  , (76, 8, "Kawala")
  , (77, 0, "Bottle Blow")
  , (78, 0, "Shakuhachi")
  , (79, 0, "Whistle")
  , (80, 0, "Ocarina")
  , (81, 0, "Square Wave")
  , (81, 1, "Square")
  , (81, 2, "Hollow Mini")
  , (81, 3, "Mellow FM")
  , (81, 4, "CC Solo")
  , (81, 5, "Shmoog")
  , (81, 6, "LM Square")
  , (81, 8, "Sine Wave")
  , (82, 0, "Saw Wave")
  , (82, 1, "Saw")
  , (82, 2, "Pulse Saw")
  , (82, 3, "Feline GR")
  , (82, 4, "Big Lead")
  , (82, 5, "Velo Lead")
  , (82, 6, "GR-300")
  , (82, 7, "LA Saw")
  , (82, 8, "Doctor Solo")
  , (82, 16, "Waspy Synth")
  , (83, 0, "Syn.Calliope")
  , (83, 1, "Vent Synth")
  , (83, 2, "Pure PanLead")
  , (84, 0, "Chiffer Lead")
  , (85, 0, "Charang")
  , (85, 8, "Dist.Lead")
  , (86, 0, "Solo Vox")
  , (87, 0, "5th Saw Wave")
  , (87, 1, "Big Fives")
  , (88, 0, "Bass & Lead")
  , (88, 1, "Big & Raw")
  , (88, 2, "Fat & Perky")
  , (89, 0, "Fantasia")
  , (89, 1, "Fantasia 2")
  , (90, 0, "Warm Pad")
  , (90, 1, "Thick Pad")
  , (90, 2, "Horn Pad")
  , (90, 3, "Rotary Strng")
  , (90, 4, "Soft Pad")
  , (91, 0, "Polysynth")
  , (91, 1, "80's PolySyn")
  , (92, 0, "Space Voice")
  , (92, 1, "Heaven II")
  , (93, 0, "Bowed Glass")
  , (94, 0, "Metal Pad")
  , (94, 1, "Tine Pad")
  , (94, 2, "Panner Pad")
  , (95, 0, "Halo Pad")
  , (96, 0, "Sweep Pad")
  , (96, 1, "Polar Pad")
  , (96, 8, "Converge")
  , (96, 9, "Shwimmer")
  , (96, 10, "Celestial Pd")
  , (97, 0, "Ice Rain")
  , (97, 1, "Harmo Rain")
  , (97, 2, "African wood")
  , (97, 8, "Clavi Pad")
  , (98, 0, "Soundtrack")
  , (98, 1, "Ancestral")
  , (98, 2, "Prologue")
  , (98, 8, "Rave")
  , (99, 0, "Crystal")
  , (99, 1, "Syn Mallet")
  , (99, 2, "Soft Crystal")
  , (99, 3, "Round Glock")
  , (99, 4, "Loud Glock")
  , (99, 5, "GlockenChime")
  , (99, 6, "Clear Bells")
  , (99, 7, "ChristmasBel")
  , (99, 8, "Vibra Bells")
  , (99, 9, "Digi Bells")
  , (99, 16, "Choral Bells")
  , (99, 17, "Air Bells")
  , (99, 18, "Bell Harp")
  , (99, 19, "Gamelimba")
  , (100, 0, "Atmosphere")
  , (100, 1, "Warm Atomos")
  , (100, 2, "Nylon Harp")
  , (100, 3, "Harpvox")
  , (100, 4, "HollowReleas")
  , (100, 5, "Nylon+Rhodes")
  , (100, 6, "Ambient Pad")
  , (101, 0, "Brightness")
  , (102, 0, "Goblin")
  , (102, 1, "Goblinson")
  , (102, 2, "50's Sci-Fi")
  , (103, 0, "Echo Drops")
  , (103, 1, "Echo Bell")
  , (103, 2, "Echo Pan")
  , (103, 3, "Echo Pan 2")
  , (103, 4, "Big Panner")
  , (103, 5, "Reso Panner")
  , (103, 6, "Water Piano")
  , (104, 0, "Star Theme")
  , (104, 1, "Star Theme 2")
  , (105, 0, "Sitar")
  , (105, 1, "Sitar 2")
  , (105, 2, "Detune Sitar  2")
  , (105, 8, "Tambra")
  , (105, 16, "Tamboura")
  , (106, 0, "Banjo")
  , (106, 1, "Muted Banjo")
  , (106, 8, "Rabab")
  , (106, 16, "Gopichant")
  , (106, 24, "Oud")
  , (107, 0, "Shamisen")
  , (107, 1, "Tsugaru")
  , (108, 0, "Koto")
  , (108, 8, "Taisho Koto")
  , (108, 16, "Kanoon")
  , (109, 0, "Kalimba")
  , (110, 0, "Bagpipe")
  , (111, 0, "Fiddle")
  , (112, 0, "Shanai")
  , (112, 1, "Shanai 2")
  , (112, 8, "Pungi")
  , (112, 16, "Hichiriki")
  , (113, 0, "Tinkle Bell")
  , (113, 8, "Bonang")
  , (113, 9, "Gender")
  , (113, 10, "Gamelan Gong")
  , (113, 11, "St.Gamelan")
  , (113, 16, "RAMA Cymbal")
  , (114, 0, "Agogo")
  , (114, 8, "Atarigane")
  , (115, 0, "Steel Drums")
  , (116, 0, "Woodblock")
  , (116, 8, "Castanets")
  , (117, 0, "Taiko")
  , (117, 8, "Concert BD")
  , (118, 0, "Melo. Tom 1")
  , (118, 1, "Real Tom")
  , (118, 8, "Melo. Tom 2")
  , (118, 9, "Rock Tom")
  , (119, 0, "Synth Drum")
  , (119, 8, "808 Tom")
  , (119, 9, "Elec Perc")
  , (120, 0, "Reverse Cym.")
  , (120, 1, "Reverse Cym2")
  , (120, 8, "Rev.Snare 1")
  , (120, 9, "Rev.Snare 2")
  , (120, 16, "Rev.Kick 1")
  , (120, 17, "Rev.ConBD")
  , (120, 24, "Rev.Tom 1")
  , (120, 25, "Rev.Tom 2")
  , (121, 0, "Gt.FretNoise")
  , (121, 1, "Gt.Cut Noise")
  , (121, 2, "String Slap")
  , (121, 3, "Gt.CutNoise2")
  , (121, 4, "Dist.CutNoise")
  , (121, 5, "Bass Slide")
  , (121, 6, "Pick Scrape")
  , (122, 0, "Breath Noise")
  , (122, 1, "Fl.Key Click")
  , (123, 0, "Seashore")
  , (123, 1, "Rain")
  , (123, 2, "Thunder")
  , (123, 3, "Wind")
  , (123, 4, "Stream")
  , (123, 5, "Bubble")
  , (124, 0, "Bird")
  , (124, 1, "Dog")
  , (124, 2, "Horse-Gallop")
  , (124, 3, "Bird 2")
  , (124, 4, "Kitty")
  , (124, 5, "Growl")
  , (125, 0, "Telephone 1")
  , (125, 1, "Telephone 2")
  , (125, 2, "DoorCreaking")
  , (125, 3, "Door")
  , (125, 4, "Scratch")
  , (125, 5, "Wind Chimes")
  , (125, 7, "Scratch 2")
  , (126, 0, "Helicopter")
  , (126, 1, "Car-Engine")
  , (126, 2, "Car-Stop")
  , (126, 3, "Car-Pass")
  , (126, 4, "Car-Crash")
  , (126, 5, "Siren")
  , (126, 6, "Train")
  , (126, 7, "Jetplane")
  , (126, 8, "Starship")
  , (126, 9, "Burst Noise")
  , (127, 0, "Applause")
  , (127, 1, "Laughing")
  , (127, 2, "Screaming")
  , (127, 3, "Punch")
  , (127, 4, "Heart Beat")
  , (127, 5, "Footsteps")
  , (127, 6, "Applause 2")
  , (128, 0, "Gun Shot")
  , (128, 1, "Machine Gun")
  , (128, 2, "Lasergun")
  , (128, 3, "Explosion")
  ]

standardSet1 :: Seq (Int, Text)
standardSet1 =
  [ (25, "Snare Roll")
  , (26, "Finger Snap")
  , (27, "High Q")
  , (28, "Slap")
  , (29, "Scratch Push")
  , (30, "Scratch Pull")
  , (31, "Sticks")
  , (32, "Square Click")
  , (33, "Metronome Click")
  , (34, "Metronome Bell")
  , (35, "Standard 1 Kick 2")
  , (36, "Standard 1 Kick 1")
  , (37, "Side Stick")
  , (38, "Standard 1 Snare 1")
  , (39, "Hand Clap")
  , (40, "Standard 1 Snare 2")
  , (41, "Low Tom2")
  , (42, "Closed Hi-hat1")
  , (43, "Low Tom1")
  , (44, "Pedal Hi-hat")
  , (45, "Mid Tom2")
  , (46, "Open Hi-hat1")
  , (47, "Mid Tom1")
  , (48, "High Tom2")
  , (49, "Crash Cymbal1")
  , (50, "High Tom1")
  , (51, "Ride Cymbal1")
  , (52, "Chinese Cymbal")
  , (53, "Ride Bell")
  , (54, "Tambourine")
  , (55, "Splash Cymbal")
  , (56, "Cowbell")
  , (57, "Crash Cymbal2")
  , (58, "Vibra-slap")
  , (59, "Ride Cymbal2")
  , (60, "High Bongo")
  , (61, "Low Bongo")
  , (62, "Mute High Conga")
  , (63, "Open High Conga")
  , (64, "Low Conga")
  , (65, "High Timbale")
  , (66, "Low Timbale")
  , (67, "High Agogo")
  , (68, "Low Agogo")
  , (69, "Cabasa")
  , (70, "Maracas")
  , (71, "Short Hi Whistle")
  , (72, "Long Low Whistle")
  , (73, "Short Guiro")
  , (74, "Long Guiro")
  , (75, "Claves")
  , (76, "High Wood Block")
  , (77, "Low Wood Block")
  , (78, "Mute Cuica")
  , (79, "Open Cuica")
  , (80, "Mute Triangle")
  , (81, "Open Triangle")
  , (82, "Shaker")
  , (83, "Jingle Bell")
  , (84, "Bell Tree")
  , (85, "Castanets")
  , (86, "Mute Surdo")
  , (87, "Open Surdo")
  ]

override :: (Ord k) => Seq (k, v) -> Seq (k, v) -> Seq (k, v)
override defs overs = Seq.fromList (Map.toList (Map.fromList (toList overs) <> Map.fromList (toList defs)))

overrideStandardSet1 :: Seq (Int, Text) -> Seq (Int, Text)
overrideStandardSet1 = override standardSet1

standardSet2 :: Seq (Int, Text)
standardSet2 =
  overrideStandardSet1
    [ (35, "Standard 2 Kick 2")
    , (36, "Standard 2 Kick 1")
    , (38, "Standard 2 Snare 1")
    , (40, "Standard 2 Snare 2")
    , (42, "Closed Hi-hat2")
    , (46, "Open Hi-hat2")
    , (84, "Bar Chimes")
    ]

roomSet :: Seq (Int, Text)
roomSet =
  overrideStandardSet1
    [ (35, "Room Kick 2")
    , (36, "Room Kick 1")
    , (38, "Room Snare 1")
    , (40, "Room Snare 2")
    , (41, "Room Low Tom2")
    , (42, "Closed Hi-hat3")
    , (43, "Room Low Tom1")
    , (45, "Room Mid Tom2")
    , (46, "Open Hi-hat2")
    , (47, "Room Mid Tom1")
    , (50, "Room Hi Tom1")
    ]

drumSets :: Seq (Int, Text, Seq (Int, Text))
drumSets =
  [ (1, "STANDARD Set1", standardSet1)
  , (2, "STANDARD Set2", standardSet2)
  , (9, "ROOM Set", roomSet)
  ]

data DrumSet = DrumSet
  { dsIx :: !Int
  , dsProg :: !Int
  , dsName :: !Text
  , dsHits :: !(Seq (Int, Text))
  }
  deriving stock (Eq, Ord, Show)

mkDrumSet :: Int -> (Int, Text, Seq (Int, Text)) -> DrumSet
mkDrumSet i (p, n, hs) = DrumSet i p n hs

findDrumSet :: Text -> Maybe DrumSet
findDrumSet = findThing (\(_, n, _) -> n) mkDrumSet drumSets

firstDrumSet :: DrumSet
firstDrumSet = mkDrumSet 0 (Seq.index drumSets 0)

nextDrumSet :: DrumSet -> DrumSet
nextDrumSet (DrumSet i _ _ _) =
  if i >= Seq.length drumSets
    then firstDrumSet
    else let j = i + 1 in mkDrumSet j (Seq.index drumSets j)

-- setSoundNamed :: Int -> Text -> Seq LiveMsg
-- setSoundNamed chan frag =
--   case findInstrument frag of
--     Just (Inst _ prog var _) -> setSound chan (prog - 1) var
--     Nothing -> error ("Could not find instrument: " <> T.unpack frag)
