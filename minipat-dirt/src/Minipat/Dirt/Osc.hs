{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Osc
  ( DatumProxy (..)
  , datumProxyType
  , Timed (..)
  , Attrs
  , attrs
  , IsAttrs (..)
  , Attr (..)
  , PlayErr (..)
  , PlayEnv (..)
  , convertEvent
  , convertTape
  , playPacket
  , handshakePacket
  )
where

import Control.Exception (Exception)
import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Dahdit.Midi.Osc (Datum (..), DatumType (..), IsDatum (..), Msg (..), Packet (..))
import Dahdit.Midi.OscAddr (RawAddrPat)
import Data.Foldable (foldl')
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Minipat.Stream (Ev (..), Tape, tapeToList)
import Minipat.Time (CycleDelta (..), CycleTime (..), Span, spanCycle, spanDelta)
import Nanotime (PosixTime, TimeDelta (..), addTime, timeDeltaFromFracSecs, timeDeltaToNanos)

data DatumProxy a where
  DatumProxyInt32 :: DatumProxy Int32
  DatumProxyInt64 :: DatumProxy Int64
  DatumProxyFloat :: DatumProxy Float
  DatumProxyDouble :: DatumProxy Double
  DatumProxyString :: DatumProxy Text

datumProxyType :: DatumProxy a -> DatumType
datumProxyType = \case
  DatumProxyInt32 -> DatumTypeInt32
  DatumProxyInt64 -> DatumTypeInt64
  DatumProxyFloat -> DatumTypeFloat
  DatumProxyDouble -> DatumTypeDouble
  DatumProxyString -> DatumTypeString

data Timed a = Timed
  { timedKey :: !PosixTime
  , timedVal :: !a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type Attrs = Map Text Datum

attrs :: [(Text, Datum)] -> Attrs
attrs = Map.fromList

class IsAttrs a where
  toAttrs :: a -> Attrs

instance IsAttrs Attrs where
  toAttrs = id

data Attr a = Attr
  { attrKey :: !Text
  , attrVal :: !a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (IsDatum a) => IsAttrs (Attr a) where
  toAttrs (Attr k v) = Map.singleton k (toDatum v)

namedPayload :: Attrs -> Seq Datum
namedPayload = foldl' go Empty . Map.toList
 where
  go !acc (k, v) = acc :|> DatumString k :|> v

data PlayErr
  = PlayErrDupe !Text
  | PlayErrLate
  | PlayErrCont
  deriving stock (Eq, Ord, Show)

instance Exception PlayErr

type M = Either PlayErr

insertSafe :: Text -> Datum -> Attrs -> M Attrs
insertSafe k v m =
  case Map.lookup k m of
    Nothing -> pure (Map.insert k v m)
    Just _ -> throwError (PlayErrDupe k)

replaceAliases :: [(Text, Text)] -> Attrs -> M Attrs
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
  , ("hpq", "hresonance")
  , ("bpf", "bandf")
  , ("bpq", "bandq")
  , ("res", "resonance")
  , ("midi", "midinote")
  , ("n", "midinote")
  , ("oct", "octave")
  , ("accel", "accelerate")
  , ("leg", "legato")
  , ("delayt", "delaytime")
  , ("delayfb", "delayfeedback")
  , ("phasr", "phaserrate")
  , ("phasdp", "phaserdepth")
  , ("tremr", "tremolorate")
  , ("tremdp", "tremolodepth")
  , ("dist", "distort")
  , ("o", "orbit")
  , ("ts", "timescale")
  , ("s", "sound")
  ]

spanCycleM :: Span -> M CycleTime
spanCycleM = maybe (throwError PlayErrLate) pure . spanCycle

spanDeltaM :: Span -> M CycleDelta
spanDeltaM = maybe (throwError PlayErrCont) pure . spanDelta

data PlayEnv = PlayEnv
  { peStart :: !PosixTime
  , peCycle :: !Integer
  , peCps :: !Rational
  }
  deriving stock (Eq, Ord, Show)

timeDeltaToMicros :: TimeDelta -> Float
timeDeltaToMicros td =
  let (_, ns) = timeDeltaToNanos td
  in  fromIntegral ns / 1000

convertEvent :: (IsAttrs a) => PlayEnv -> Ev a -> M (Timed Attrs)
convertEvent (PlayEnv startTime startCyc cps) (Ev sp dat) = do
  targetCyc <- fmap unCycleTime (spanCycleM sp)
  let cycOffset = targetCyc - fromInteger startCyc
      onset = addTime startTime (timeDeltaFromFracSecs (cycOffset / cps))
  deltaCyc <- fmap unCycleDelta (spanDeltaM sp)
  let deltaTime = timeDeltaToMicros (timeDeltaFromFracSecs (deltaCyc / cps))
  dat' <- replaceAliases playAliases (toAttrs dat)
  dat'' <- insertSafe "delta" (DatumFloat deltaTime) dat'
  dat''' <- insertSafe "cps" (DatumFloat (realToFrac cps)) dat''
  pure (Timed onset dat''')

convertTape :: (IsAttrs a) => PlayEnv -> Tape a -> M (Seq (Timed Attrs))
convertTape penv = traverse (convertEvent penv) . Seq.fromList . tapeToList

playAddr :: RawAddrPat
playAddr = "/dirt/play"

playPacket :: Attrs -> Packet
playPacket ats = PacketMsg (Msg playAddr (namedPayload ats))

handshakeAddr :: RawAddrPat
handshakeAddr = "/dirt/handshake"

handshakePacket :: Packet
handshakePacket = PacketMsg (Msg handshakeAddr Empty)
