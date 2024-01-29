{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Osc where

import Control.Exception (Exception)
import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Dahdit.Midi.Osc (Datum (..), Msg (..), Packet (..))
import Dahdit.Midi.OscAddr (RawAddrPat)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Minipat.Dirt.Resources (Timed (..))
import Minipat.Stream (Ev (..), Tape, tapeToList)
import Minipat.Time qualified as T
import Nanotime (PosixTime, TimeDelta (..), addTime, timeDeltaFromFracSecs, timeDeltaToNanos)

type OscMap = Map Text Datum

namedPayload :: OscMap -> Seq Datum
namedPayload = foldl' go Empty . Map.toList
 where
  go !acc (k, v) = acc :|> DatumString k :|> v

data OscErr
  = OscErrDupe !Text
  | OscErrLate
  | OscErrCont
  deriving stock (Eq, Ord, Show)

instance Exception OscErr

type M = Either OscErr

insertSafe :: Text -> Datum -> OscMap -> M OscMap
insertSafe k v m =
  case Map.lookup k m of
    Nothing -> pure (Map.insert k v m)
    Just _ -> throwError (OscErrDupe k)

replaceAliases :: [(Text, Text)] -> OscMap -> M OscMap
replaceAliases as m0 = foldM go m0 as
 where
  go !m (x, y) = do
    case Map.lookup x m of
      Nothing -> pure m
      Just v -> insertSafe y v (Map.delete x m)

-- Useful params:
-- sound - string, req - name of sound
-- orbit - int, opt - index of orbit
-- cps - float, given - current cps
-- cycle - float, given - event start in cycle time
-- delta - float, given - microsecond length of event
playAliases :: [(Text, Text)]
playAliases =
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
  , ("s", "sound")
  ]

spanCycleM :: T.Span -> M Rational
spanCycleM = maybe (throwError OscErrLate) pure . T.spanCycle

spanDeltaM :: T.Span -> M Rational
spanDeltaM = maybe (throwError OscErrCont) pure . T.spanDelta

data PlayEnv = PlayEnv
  { peStart :: !PosixTime
  , peCycle :: !Integer
  , peCps :: !Rational
  }
  deriving stock (Eq, Ord, Show)

data PlayEvent = PlayEvent
  { peTime :: !PosixTime
  , peData :: !OscMap
  }
  deriving stock (Eq, Ord, Show)

timeDeltaToMicros :: TimeDelta -> Float
timeDeltaToMicros td =
  let (_, ns) = timeDeltaToNanos td
  in  fromIntegral ns / 1000

convertEvent :: PlayEnv -> Ev OscMap -> M PlayEvent
convertEvent (PlayEnv startTime startCyc cps) (Ev sp dat) = do
  targetCyc <- spanCycleM sp
  let cycOffset = targetCyc - fromInteger startCyc
      onset = addTime startTime (timeDeltaFromFracSecs (cycOffset / cps))
  deltaCyc <- spanDeltaM sp
  let deltaTime = timeDeltaToMicros (timeDeltaFromFracSecs (deltaCyc / cps))
  dat' <- replaceAliases playAliases dat
  dat'' <- insertSafe "delta" (DatumFloat deltaTime) dat'
  pure (PlayEvent onset dat'')

convertTape :: PlayEnv -> Tape OscMap -> M (Seq PlayEvent)
convertTape penv = traverse (convertEvent penv) . Seq.fromList . tapeToList

playAddr :: RawAddrPat
playAddr = "/dirt/play"

playPacket :: PlayEvent -> Timed Packet
playPacket (PlayEvent time dat) =
  let pkt = PacketMsg (Msg playAddr (namedPayload dat))
  in  Timed time pkt

handshakeAddr :: RawAddrPat
handshakeAddr = "/dirt/handshake"

handshakePacket :: Packet
handshakePacket = PacketMsg (Msg handshakeAddr Empty)
