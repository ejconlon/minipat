{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Osc where

import Control.Exception (Exception)
import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (MonadState (..), StateT, execStateT)
import Control.Monad.Trans (lift)
import Dahdit.Midi.Osc (Bundle (..), Datum (..), Msg (..), Packet (..))
import Dahdit.Midi.OscAddr (RawAddrPat)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Minipat.Base qualified as B
import Minipat.Time qualified as T
import Nanotime (PosixTime, TimeDelta, addTime, posixToNtp)

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

modSt :: (Monad m) => (s -> m s) -> StateT s m ()
modSt f = get >>= lift . f >>= put

datFloat :: Rational -> Datum
datFloat = DatumFloat . fromRational

evToPayload :: Rational -> B.Ev OscMap -> M OscMap
evToPayload _cps (B.Ev sp dat0) = flip execStateT dat0 $ do
  modSt $ replaceAliases playAliases
  -- modSt $ insertSafe "cps" (datFloat cps)
  -- modSt $ \dat -> do
  --   cyc <- spanCycleM sp
  --   insertSafe "cycle" (datFloat cyc) dat
  modSt $ \dat -> do
    del <- spanDeltaM sp
    insertSafe "delta" (datFloat del) dat

-- Each time delta is against origin
tapeToPayloads :: Rational -> B.Tape OscMap -> M (Maybe (Rational, Seq (TimeDelta, OscMap)))
tapeToPayloads cps tape = go1
 where
  go1 = case B.tapeToList tape of
    [] -> pure Nothing
    evs@(B.Ev sp _ : _) -> do
      origin <- spanCycleM sp
      go2 origin Empty evs
  go2 !origin !acc = \case
    [] -> pure (Just (origin, acc))
    ev@(B.Ev sp _) : evs' -> do
      target <- spanCycleM sp
      let td = T.relDelta cps origin target
      pl <- evToPayload cps ev
      go2 origin (acc :|> (td, pl)) evs'

playAddr :: RawAddrPat
playAddr = "/dirt/play"

data PlayRecord = PlayRecord
  { prDawn :: !PosixTime
  , prCps :: !Rational
  , prTape :: !(B.Tape OscMap)
  }
  deriving stock (Eq, Ord, Show)

playPkt :: PlayRecord -> M (Maybe Packet)
playPkt (PlayRecord dawn cps tape) = go1
 where
  go1 = do
    flip fmap (tapeToPayloads cps tape) $ \case
      Nothing -> Nothing
      Just (originCy, pls) ->
        let originTy = addTime dawn (T.cycleToDelta cps originCy)
            subPkts = fmap (go2 originTy) pls
        in  Just (PacketBundle (Bundle (posixToNtp originTy) subPkts))
  go2 originTy (td, pl) =
    let pkt = PacketMsg (Msg playAddr (namedPayload pl))
    in  if td == 0
          then pkt
          else
            let ty = posixToNtp (addTime originTy td)
            in  PacketBundle (Bundle ty (Seq.singleton pkt))

handshakeAddr :: RawAddrPat
handshakeAddr = "/dirt/handshake"

handshakePkt :: Packet
handshakePkt = PacketMsg (Msg handshakeAddr Empty)
