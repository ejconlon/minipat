{-# LANGUAGE OverloadedStrings #-}

-- | Converts attrs to MIDI events
module Minipat.Midi.Convert where

import Dahdit.Midi.Midi (ChanData (..), ChanVoiceData (..))
import Dahdit.Midi.Osc (Datum (..))
import Data.Default (def)
import Data.Functor ((<&>))
import Data.Int (Int32)
import Minipat.Live.Attrs (Attrs, ToAttrs (..), attrsSingleton)
import Minipat.Live.Convert (Branch (..), ConvErr, ConvM, branchM, defaultM, lookupM, runConvM)
import Minipat.Live.Datum (DatumProxy (..))
import Minipat.Midi.Midi (PortData (..), psFromText)

newtype Vel = Vel {unVel :: Int32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Integral, Real, Enum)

instance ToAttrs Vel where
  toAttrs (Vel x) = attrsSingleton "vel" (DatumInt32 x)

-- TODO support more:
-- legato to change note on/off sort order
-- program change
-- control change
-- the rest of ChanDataVoice
convertMidiAttrs :: Attrs -> Either ConvErr PortData
convertMidiAttrs = runConvM rootM

-- Default velocity in something like Ableton is 100
defVel :: Int32
defVel = 100

rootM :: ConvM PortData
rootM = do
  port <- fmap (maybe def psFromText) (lookupM "port" DatumProxyString)
  branchM @[]
    [
      ( "note"
      , Branch DatumProxyInt32 $ \(fromIntegral -> note) -> do
          vel <- defaultM "vel" DatumProxyInt32 defVel <&> fromIntegral
          pure (PortData port (ChanDataVoice (ChanVoiceDataNoteOn (note + 60) vel)))
      )
    ]
