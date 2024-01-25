{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Osc where

import Data.Foldable (foldl', for_)
import Control.Exception (Exception)
import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Dahdit.Midi.Osc (Datum (..), Msg (..), Bundle (..), Packet (..))
import Dahdit.Midi.OscAddr (RawAddrPat)
import Minipat.Base qualified as B
import Minipat.Time qualified as T
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Control.Monad.State.Strict (StateT, execStateT, MonadState (..), modify')
import Control.Monad.Trans (lift)
import Control.Monad.Except (throwError)
import Nanotime (PosixTime, TimeDelta, addTime, posixToNtp)

data OscErr =
    OscErrReq !Text
  | OscErrDupe !Text
  | OscErrLate
  | OscErrCont
  deriving stock (Eq, Ord, Show)

instance Exception OscErr

type M = Either OscErr

type PrePayload = Map Text Datum

guardRequired :: [Text] -> Map Text a -> M ()
guardRequired rs m = for_ rs $ \r ->
  maybe (Left (OscErrReq r)) (const (Right ())) (Map.lookup r m)

replaceAliases :: [(Text, Text)] -> Map Text a -> M (Map Text a)
replaceAliases as m0 = foldM go m0 as where
  go !m (x, y) =
    case Map.lookup x m of
      Nothing -> Right m
      Just v -> case Map.lookup y m of
        Nothing -> Right (Map.insert y v (Map.delete x m))
        Just _ -> Left (OscErrDupe y)

data Endpoint = Endpoint
  { epAddr :: !RawAddrPat
  , epReq :: ![Text]
  , epProvided :: ![Text]
  , epAliases :: ![(Text, Text)]
  } deriving stock (Eq, Ord, Show)

-- Useful params:
-- sound - string, req - name of sound
-- orbit - int, opt - index of orbit
-- cps - float - current cps
-- cycle - float - event start in cycle time
-- delta - float - microsecond length of event
playEp :: Endpoint
playEp = Endpoint
  { epAddr = "/dirt/play"
  , epReq =
    [ "sound"
    , "orbit"
    ]
  , epProvided =
    [ "delta"
    , "cps"
    , "cycle"
    ]
  , epAliases =
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
  }

modSt :: Monad m => (s -> m s) -> StateT s m ()
modSt f = get >>= lift . f >>= put

onSt :: Monad m => (s -> m ()) -> StateT s m ()
onSt f = get >>= lift . f

insSt :: (Monad m, Ord k) => k -> m v -> StateT (Map k v) m ()
insSt k mv = lift mv >>= modify' . Map.insert k

datFloat :: Rational -> Datum
datFloat = DatumFloat . fromRational

spanCycleM :: T.Span -> M Rational
spanCycleM = maybe (throwError OscErrLate) pure . T.spanCycle

evToPayload :: Rational -> B.Ev PrePayload -> M PrePayload
evToPayload cps (B.Ev sp m0) = flip execStateT m0 $ do
  modSt (replaceAliases (epAliases playEp))
  onSt (guardRequired (epReq playEp))
  insSt "cps" (pure (datFloat cps))
  insSt "cycle" (fmap (datFloat . (/ 1000)) (spanCycleM sp))
  insSt "delta" (maybe (throwError OscErrCont) (pure . datFloat) (T.spanDelta sp))

-- Each time delta is against origin
tapeToPayloads :: Rational -> B.Tape PrePayload -> M (Maybe (Rational, Seq (TimeDelta, PrePayload)))
tapeToPayloads cps tape = go1 where
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

namedPayload :: PrePayload -> Seq Datum
namedPayload = foldl' go Empty . Map.toList where
  go !acc (k, v) = acc :|> DatumString k :|> v

playPkt :: Rational -> B.Tape PrePayload -> PosixTime -> M (Maybe Packet)
playPkt cps tape dawn = go1 where
  go1 = do
    flip fmap (tapeToPayloads cps tape) $ \case
      Nothing -> Nothing
      Just (originCy, pls) ->
        let originTy = addTime dawn (T.cycleToDelta cps originCy)
            subPkts = fmap (go2 originTy) pls
        in Just (PacketBundle (Bundle (posixToNtp originTy) subPkts))
  go2 originTy (td, pl) =
    let pkt = PacketMsg (Msg playAddr (namedPayload pl))
    in if td == 0
      then pkt
      else
        let ty = posixToNtp (addTime originTy td)
        in PacketBundle (Bundle ty (Seq.singleton pkt))

handshakeAddr :: RawAddrPat
handshakeAddr = "/dirt/handshake"

handshakePkt :: Packet
handshakePkt = PacketMsg (Msg handshakeAddr Empty)
