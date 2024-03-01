{-# LANGUAGE OverloadedStrings #-}

module Minipat.Midi.Convert where

import Dahdit.Midi.Midi (ChanData (..), ChanVoiceData (..))
import Dahdit.Midi.Osc (Datum (..))
import Data.Functor ((<&>))
import Data.Int (Int32)
import Minipat.Live.Attrs (Attrs, IsAttrs (..), attrsSingleton)
import Minipat.Live.Convert (ConvErr, getM, lookupDefaultM, matchM, runConvM)
import Minipat.Live.Datum (DatumProxy (..))

newtype Vel = Vel {unVel :: Int32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Integral, Real, Enum)

instance IsAttrs Vel where
  toAttrs (Vel x) = attrsSingleton "vel" (DatumInt32 x)

convertMidiAttrs :: Attrs -> Either ConvErr ChanData
convertMidiAttrs =
  -- Default velocity in something like Ableton is 100
  let defVelDatum = DatumInt32 100
  in  runConvM $ do
        note <- getM "note" >>= matchM DatumProxyInt32 <&> fromIntegral
        vel <- lookupDefaultM defVelDatum "vel" >>= matchM DatumProxyInt32 <&> fromIntegral
        pure (ChanDataVoice (ChanVoiceDataNoteOn note vel))
