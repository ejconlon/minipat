{-# LANGUAGE OverloadedStrings #-}

-- | Converts attrs to MIDI events
module Minipat.Midi.Convert where

import Dahdit.Midi.Midi (ChanData (..), ChanVoiceData (..))
import Dahdit.Midi.Osc (Datum (..))
import Data.Functor ((<&>))
import Data.Int (Int32)
import Minipat.Live.Attrs (Attrs, IsAttrs (..), attrsSingleton)
import Minipat.Live.Convert (ConvErr, defaultM, getM, runConvM)
import Minipat.Live.Datum (DatumProxy (..))

newtype Vel = Vel {unVel :: Int32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Integral, Real, Enum)

instance IsAttrs Vel where
  toAttrs (Vel x) = attrsSingleton "vel" (DatumInt32 x)

-- TODO support more:
-- legato to change note on/off sort order
-- program change
-- control change
-- the rest of ChanDataVoice
convertMidiAttrs :: Attrs -> Either ConvErr ChanData
convertMidiAttrs =
  -- Default velocity in something like Ableton is 100
  let defVel = 100
  in  runConvM $ do
        note <- getM "note" DatumProxyInt32 <&> fromIntegral
        vel <- defaultM "vel" DatumProxyInt32 defVel <&> fromIntegral
        pure (ChanDataVoice (ChanVoiceDataNoteOn note vel))
