{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Midi message makers for the Akai MPK mini
module Minipat.Midi.Mpk where

import Control.Monad.ST (runST)
import Dahdit
import Dahdit.Midi.Midi (LiveMsg (..), Manf (..), ManfSysEx (..), SysExData (..))
import Data.ByteString.Short qualified as BSS
import Data.Default (Default (..))
import Data.String (IsString)

akaiManf :: Manf
akaiManf = ManfShort 0x47

clamp :: Word8 -> Word8 -> Word8
clamp = flip min

data ZeroByte = ZeroByte
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (BinaryRep Word8) via (ViaBoundedEnum Word8 ZeroByte)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 ZeroByte)

instance Default ZeroByte where
  def = ZeroByte

data Prog = ProgRam | ProgBank !Int
  deriving stock (Eq, Ord, Show)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 Prog)

instance BinaryRep Word8 Prog where
  toBinaryRep = \case
    ProgRam -> 0
    ProgBank n -> clamp 8 (fromIntegral n)
  fromBinaryRep =
    Right . \case
      0 -> ProgRam
      n -> ProgBank (fromIntegral (clamp 8 n))

instance Default Prog where
  def = ProgRam

newtype ShortName = ShortName {unShortName :: StaticBytes 16}
  deriving stock (Show)
  deriving newtype (Eq, Ord, StaticByteSized, Binary, IsString)

instance Default ShortName where
  def = "undefined"

data ProgConfig = ProgConfig
  { pcName :: !ShortName
  , pcMidiChan :: !Word8
  , pcAftertouch :: !Aftertouch
  , pcKeybedControl :: !KeybedControl
  , pcOctave :: !Octave
  , pcArpConfig :: !ArpConfig
  , pcJsConfig :: !JsConfig
  , pcPadBankA :: !(StaticSeq 8 Pad)
  , pcPadBankB :: !(StaticSeq 8 Pad)
  , pcKnobs :: !(StaticSeq 8 Knob)
  , pcTranspose :: !Transpose
  }
  deriving stock (Eq, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric ProgConfig)

instance Default ProgConfig where
  def = ProgConfig def def def def def def def def def def def

data Aftertouch
  = AftertouchOff
  | AftertouchChan
  | AftertouchPoly
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (BinaryRep Word8) via (ViaBoundedEnum Word8 Aftertouch)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 Aftertouch)

instance Default Aftertouch where
  def = AftertouchOff

-- TODO figure this one out
data KeybedControl
  = KeybedControl0
  | KeybedControl1
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (BinaryRep Word8) via (ViaBoundedEnum Word8 KeybedControl)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 KeybedControl)

instance Default KeybedControl where
  def = KeybedControl0

newtype Octave = Octave {unOctave :: Int8}
  deriving stock (Show)
  deriving newtype (Eq, Ord)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 Octave)

instance Default Octave where
  def = Octave 0

instance BinaryRep Word8 Octave where
  toBinaryRep = clamp 8 . fromIntegral . (+ 4) . unOctave
  fromBinaryRep = Right . Octave . subtract 4 . fromIntegral . clamp 8

data ArpConfig = ArpConfig
  { acStatus :: !ArpStatus
  , acMode :: !ArpMode
  , acTimeDiv :: !ArpTimeDiv
  , acClock :: !ArpClock
  , acLatch :: !ArpLatch
  , acSwing :: !ArpSwing
  , acTempoTaps :: !ArpTempoTaps
  , acZeroByte :: !ZeroByte
  , acTempo :: !Word8
  , asOctave :: !Word8
  }
  deriving stock (Eq, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric ArpConfig)

instance Default ArpConfig where
  def = ArpConfig def def def def def def def def def def

data Pad = Pad
  { padNote :: !Word8
  , padCC :: !Word8
  , padPC :: !Word8
  }
  deriving stock (Eq, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric Pad)

instance Default Pad where
  def = Pad def def def

data KnobMode
  = KnobModeAbs
  | KnobModeRel
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (BinaryRep Word8) via (ViaBoundedEnum Word8 KnobMode)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 KnobMode)

instance Default KnobMode where
  def = KnobModeAbs

data Knob = Knob
  { knobMode :: !KnobMode
  , knobCC :: !Word8
  , knobLow :: !Word8
  , knobHigh :: !Word8
  , knobName :: !ShortName
  }
  deriving stock (Eq, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric Knob)

instance Default Knob where
  def = Knob def def def def def

data ArpStatus
  = ArpStatusDisabled
  | ArpStatusEnabled
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 ArpStatus)

instance BinaryRep Word8 ArpStatus where
  toBinaryRep = \case
    ArpStatusDisabled -> 0
    ArpStatusEnabled -> 127
  fromBinaryRep =
    Right . \case
      0 -> ArpStatusDisabled
      _ -> ArpStatusEnabled

instance Default ArpStatus where
  def = ArpStatusDisabled

data ArpMode
  = ArpModeUp
  | ArpModeDown
  | ArpModeExcl
  | ArpModeIncl
  | ArpModeOrdr
  | ArpModeRand
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (BinaryRep Word8) via (ViaBoundedEnum Word8 ArpMode)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 ArpMode)

instance Default ArpMode where
  def = ArpModeUp

data ArpClock
  = ArpClockInternal
  | ArpClockExternal
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (BinaryRep Word8) via (ViaBoundedEnum Word8 ArpClock)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 ArpClock)

instance Default ArpClock where
  def = ArpClockInternal

-- 2, 3, or 4
newtype ArpTempoTaps = ArpTempoTaps {unArpTempoTaps :: Word8}
  deriving stock (Show)
  deriving newtype (Eq, Ord, StaticByteSized, Binary)

instance Default ArpTempoTaps where
  def = ArpTempoTaps 4

data ArpLatch
  = ArpLatchOn
  | ArpLatchOff
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (BinaryRep Word8) via (ViaBoundedEnum Word8 ArpLatch)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 ArpLatch)

instance Default ArpLatch where
  def = ArpLatchOn

data ArpTimeDiv
  = ArpTimeDiv4
  | ArpTimeDiv4T
  | ArpTimeDiv8
  | ArpTimeDiv8T
  | ArpTimeDiv16
  | ArpTimeDiv16T
  | ArpTimeDiv32
  | ArpTimeDiv32T
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (BinaryRep Word8) via (ViaBoundedEnum Word8 ArpTimeDiv)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 ArpTimeDiv)

instance Default ArpTimeDiv where
  def = ArpTimeDiv4

-- Swing value = 50 + wire value, so between 50 to 75 means 0 to 25
newtype ArpSwing = ArpSwing {unArpSwing :: Word8}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 ArpSwing)

instance BinaryRep Word8 ArpSwing where
  toBinaryRep = clamp 25 . subtract 50 . unArpSwing
  fromBinaryRep = Right . ArpSwing . (+ 50) . clamp 25

instance Default ArpSwing where
  def = ArpSwing 50

data JsMode
  = JsModePitchBend
  | JsModeSingleCc
  | JsModeDualCc
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (BinaryRep Word8) via (ViaBoundedEnum Word8 JsMode)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 JsMode)

instance Default JsMode where
  def = JsModePitchBend

data JsAxisConfig = JsAxisConfig
  { jacMode :: !JsMode
  , jacCC1 :: !Word8
  , jacCC2 :: !Word8
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric JsAxisConfig)

instance Default JsAxisConfig where
  def = JsAxisConfig def def def

data JsConfig = JsConfig
  { jcHoriz :: !JsAxisConfig
  , jcVert :: !JsAxisConfig
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric JsConfig)

instance Default JsConfig where
  def = JsConfig def def

newtype Transpose = Transpose {unTranspose :: Int8}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)
  deriving (StaticByteSized, Binary) via (ViaBinaryRep Word8 Transpose)

instance BinaryRep Word8 Transpose where
  toBinaryRep = clamp 24 . fromIntegral . (+ 12) . unTranspose
  fromBinaryRep = Right . Transpose . subtract 12 . fromIntegral . clamp 24

instance Default Transpose where
  def = Transpose 0

sendProgConfig :: Prog -> ProgConfig -> LiveMsg
sendProgConfig p pc = LiveMsgSysEx (SysExDataManf (ManfSysEx akaiManf body))
 where
  prefix = BSS.pack [0x7F, 0x49, 0x64, 0x01, 0x76, toBinaryRep p]
  suffix = runST (putTarget (put pc))
  body = prefix <> suffix

requestProgConfig :: Prog -> LiveMsg
requestProgConfig p =
  let body = BSS.pack [0x7F, 0x49, 0x66, 0x00, 0x01, toBinaryRep p]
  in  LiveMsgSysEx (SysExDataManf (ManfSysEx akaiManf body))
