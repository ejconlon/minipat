{-# LANGUAGE OverloadedStrings #-}

-- | Superdirt-specific implementation
module Minipat.Dirt.Impl
  ( DirtBackend (..)
  , DirtSt
  )
where

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue, newTQueueIO, writeTQueue)
import Control.Exception (SomeException, throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Dahdit.Midi.Osc (Datum (..), Msg (..), Packet (..))
import Dahdit.Midi.OscAddr (RawAddrPat)
import Dahdit.Network (Conn (..), HostPort (..), resolveAddr, runDecoder, runEncoder, udpServerConn)
import Data.Default (Default (..))
import Data.Foldable (foldl', for_)
import Data.Functor ((<&>))
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Minipat.Live.Attrs (Attrs, DupeAttrErr, attrsDefault, attrsToList, attrsTryInsert, attrsUnalias)
import Minipat.Live.Backend (Backend (..), Callback (..), PlayMeta (..), WithPlayMeta (..), pmRealLength)
import Minipat.Live.Core (St (..))
import Minipat.Live.Logger (LogAction, logError, logException, logInfo)
import Minipat.Live.Resources (acquireAwait, qhTQueue, withTimeout)
import Minipat.Time (Arc (..))
import Nanotime (PosixTime, TimeDelta, timeDeltaFromFracSecs, timeDeltaToNanos)
import Network.Socket qualified as NS

data OscConn = OscConn
  { ocTargetAddr :: !NS.SockAddr
  , ocListenConn :: !(Conn NS.SockAddr)
  }

sendPacket :: OscConn -> Packet -> IO ()
sendPacket (OscConn targetAddr (Conn _ enc)) = runEncoder enc targetAddr

recvPacket :: TimeDelta -> OscConn -> IO (Either SomeException Packet)
recvPacket timeout (OscConn _ (Conn dec _)) =
  withTimeout timeout (runDecoder dec >>= either throwIO pure . snd)

data DirtBackend = DirtBackend
  { dbTargetHp :: !HostPort
  , dbListenHp :: !HostPort
  , dbOscTimeout :: !TimeDelta
  }
  deriving stock (Eq, Ord, Show)

instance Default DirtBackend where
  def =
    DirtBackend
      { dbTargetHp = HostPort (Just "127.0.0.1") 57120
      , dbListenHp = HostPort (Just "127.0.0.1") 57129
      , dbOscTimeout = timeDeltaFromFracSecs @Double 0.1
      }

type DirtSt = St DirtBackend

data DirtData = DirtData
  { ddOscConn :: !OscConn
  , ddEventQueue :: !(TQueue (WithPlayMeta Attrs))
  , ddSendTask :: !(Async ())
  }

pwRealStart :: WithPlayMeta a -> PosixTime
pwRealStart (WithPlayMeta pm _) = arcStart (pmRealArc pm)

timeDeltaToMicros :: TimeDelta -> Float
timeDeltaToMicros td =
  let (_, ns) = timeDeltaToNanos td
  in  fromIntegral ns / 1000

attrsConvert :: [(Text, Text)] -> WithPlayMeta Attrs -> Either DupeAttrErr Attrs
attrsConvert aliases (WithPlayMeta pm attrs) = do
  let delta = timeDeltaToMicros (pmRealLength pm)
      cps = realToFrac (pmCps pm)
      orbit = pmOrbit pm
  attrsUnalias aliases attrs
    >>= attrsTryInsert "delta" (DatumFloat delta)
    >>= attrsTryInsert "cps" (DatumFloat cps)
    <&> attrsDefault "orbit" (DatumInt32 (fromInteger orbit))

-- Handshake with SuperDirt
handshake :: LogAction -> TimeDelta -> OscConn -> IO ()
handshake logger timeout oscConn = do
  logInfo logger "Handshaking ..."
  sendPacket oscConn handshakePacket
  resp <- recvPacket timeout oscConn
  case resp of
    Left err -> do
      logError logger "... handshake FAILED"
      throwIO err
    Right _ -> logInfo logger "... handshake succeeded"

instance Backend DirtBackend where
  type BackendData DirtBackend = DirtData

  backendInit (DirtBackend targetHp listenHp timeout) logger getPlayingSTM = do
    targetAddr <- liftIO (resolveAddr targetHp)
    let acqOscConn = fmap (OscConn targetAddr) (udpServerConn Nothing listenHp)
    oscConn <- acqOscConn
    liftIO (handshake logger timeout oscConn)
    eventQueue <- liftIO newTQueueIO
    let send pw = do
          case attrsConvert dirtAliases pw of
            Left err -> logException logger "Failed to convert event" err
            Right attrs -> sendPacket oscConn (playPacket attrs)
        acqSendTask = acquireAwait pwRealStart getPlayingSTM (qhTQueue eventQueue) send
    fmap (DirtData oscConn eventQueue) acqSendTask

  backendSend _ _ cb evs = runCallback cb (atomically . for_ evs . writeTQueue . ddEventQueue)

  backendClear _ _ cb = runCallback cb (atomically . void . flushTQueue . ddEventQueue)

  -- TODO really check
  backendCheck _ _ _ = pure True

namedPayload :: Attrs -> Seq Datum
namedPayload = foldl' go Empty . attrsToList
 where
  go !acc (k, v) = acc :|> DatumString k :|> v

playAddr :: RawAddrPat
playAddr = "/dirt/play"

playPacket :: Attrs -> Packet
playPacket ats = PacketMsg (Msg playAddr (namedPayload ats))

handshakeAddr :: RawAddrPat
handshakeAddr = "/dirt/handshake"

handshakePacket :: Packet
handshakePacket = PacketMsg (Msg handshakeAddr Empty)

-- Useful params:
-- sound - string, req - name of sound
-- orbit - int, opt - index of orbit
-- cps - float, given - current cps
-- cycle - float, given - event start in cycle time
-- delta - float, given - microsecond length of event
-- TODO add more aliases for params
dirtAliases :: [(Text, Text)]
dirtAliases =
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
