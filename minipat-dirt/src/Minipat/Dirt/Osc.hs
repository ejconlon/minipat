{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Osc
  ( Timed (..)
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
import Dahdit.Midi.Osc (Datum (..), Msg (..), Packet (..))
import Dahdit.Midi.OscAddr (RawAddrPat)
import Data.Foldable (foldl')
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Minipat.Dirt.Attrs (Attrs, IsAttrs (..), attrsDelete, attrsInsert, attrsLookup, attrsToList)
import Minipat.Stream (Ev (..), Tape, tapeToList)
import Minipat.Time (CycleDelta (..), CycleTime (..), Span, spanCycle, spanDelta)
import Nanotime (PosixTime (..), TimeDelta (..), addTime, timeDeltaFromFracSecs, timeDeltaToNanos)
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as P

data Timed a = Timed
  { timedKey :: !PosixTime
  , timedVal :: !a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty a) => Pretty (Timed a) where
  pretty (Timed k v) = P.hsep [pretty (unPosixTime k), pretty v]

namedPayload :: Attrs -> Seq Datum
namedPayload = foldl' go Empty . attrsToList
 where
  go !acc (k, v) = acc :|> DatumString k :|> v

data PlayErr
  = PlayErrDupe !Text
  | PlayErrCont
  deriving stock (Eq, Ord, Show)

instance Exception PlayErr

type M = Either PlayErr

insertSafe :: Text -> Datum -> Attrs -> M Attrs
insertSafe k v m =
  case attrsLookup k m of
    Nothing -> pure (attrsInsert k v m)
    Just _ -> throwError (PlayErrDupe k)

replaceAliases :: [(Text, Text)] -> Attrs -> M Attrs
replaceAliases as m0 = foldM go m0 as
 where
  go !m (x, y) = do
    case attrsLookup x m of
      Nothing -> pure m
      Just v -> insertSafe y v (attrsDelete x m)

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
  , ("n", "note")
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

spanDeltaM :: Span -> M CycleDelta
spanDeltaM = maybe (throwError PlayErrCont) pure . spanDelta

data PlayEnv = PlayEnv
  { peStart :: !PosixTime
  , peCycle :: !CycleTime
  , peCps :: !Rational
  }
  deriving stock (Eq, Ord, Show)

timeDeltaToMicros :: TimeDelta -> Float
timeDeltaToMicros td =
  let (_, ns) = timeDeltaToNanos td
  in  fromIntegral ns / 1000

convertEvent :: (IsAttrs a) => PlayEnv -> Ev a -> M (Maybe (Timed Attrs))
convertEvent (PlayEnv startTime startCyc cps) (Ev sp dat) =
  case spanCycle sp of
    Nothing ->
      -- Only emit start events
      pure Nothing
    Just targetCyc -> do
      let cycOffset = targetCyc - startCyc
          onset = addTime startTime (timeDeltaFromFracSecs (unCycleTime cycOffset / cps))
      deltaCyc <- fmap unCycleDelta (spanDeltaM sp)
      let deltaTime = timeDeltaToMicros (timeDeltaFromFracSecs (deltaCyc / cps))
      dat' <- replaceAliases playAliases (toAttrs dat)
      dat'' <- insertSafe "delta" (DatumFloat deltaTime) dat'
      dat''' <- insertSafe "cps" (DatumFloat (realToFrac cps)) dat''
      pure (Just (Timed onset dat'''))

traverseMaybe :: (Monad m) => (a -> m (Maybe b)) -> Seq a -> m (Seq b)
traverseMaybe f = go Empty
 where
  go !acc = \case
    Empty -> pure acc
    a :<| as' -> f a >>= maybe (go acc as') (\b -> go (acc :|> b) as')

convertTape :: (IsAttrs a) => PlayEnv -> Tape a -> M (Seq (Timed Attrs))
convertTape penv = traverseMaybe (convertEvent penv) . Seq.fromList . tapeToList

playAddr :: RawAddrPat
playAddr = "/dirt/play"

playPacket :: Attrs -> Packet
playPacket ats = PacketMsg (Msg playAddr (namedPayload ats))

handshakeAddr :: RawAddrPat
handshakeAddr = "/dirt/handshake"

handshakePacket :: Packet
handshakePacket = PacketMsg (Msg handshakeAddr Empty)
