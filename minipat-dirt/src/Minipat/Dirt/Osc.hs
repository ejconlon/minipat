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
import Minipat.Base qualified as B
import Minipat.Time qualified as T
import Nanotime (PosixTime, TimeDelta, addTime, timeDeltaFromFracSecs)

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
spanCycleM = maybe (throwError OscErrLate) (pure . (/ 1000)) . T.spanCycle

spanDeltaM :: T.Span -> M Rational
spanDeltaM = maybe (throwError OscErrCont) pure . T.spanDelta

data PlayEnv = PlayEnv
  { peStart :: !PosixTime
  , peCycle :: !Integer
  , peCps :: !Rational
  }
  deriving stock (Eq, Ord, Show)

data PlayEvent = PlayEvent
  { peOnset :: !PosixTime
  , peLength :: !TimeDelta
  , peData :: !OscMap
  }
  deriving stock (Eq, Ord, Show)

convertEvent :: PlayEnv -> B.Ev OscMap -> M PlayEvent
convertEvent (PlayEnv startTime startCyc cps) (B.Ev sp dat) = do
  target <- spanCycleM sp
  let cycOffset = target - fromInteger startCyc
      onset = addTime startTime (timeDeltaFromFracSecs (cps * cycOffset))
  delta <- spanDeltaM sp
  let len = timeDeltaFromFracSecs (cps * delta)
  dat' <- replaceAliases playAliases dat
  pure (PlayEvent onset len dat')

convertTape :: PlayEnv -> B.Tape OscMap -> M (Seq PlayEvent)
convertTape penv = traverse (convertEvent penv) . Seq.fromList . B.tapeToList

playAddr :: RawAddrPat
playAddr = "/dirt/play"

data TimedPacket = TimedPacket
  { tpTime :: !PosixTime
  , tpPacket :: !Packet
  }
  deriving stock (Eq, Ord, Show)

playPacket :: PlayEvent -> TimedPacket
playPacket (PlayEvent time _ dat) =
  let pkt = PacketMsg (Msg playAddr (namedPayload dat))
  in  TimedPacket time pkt

handshakeAddr :: RawAddrPat
handshakeAddr = "/dirt/handshake"

handshakePacket :: Packet
handshakePacket = PacketMsg (Msg handshakeAddr Empty)
