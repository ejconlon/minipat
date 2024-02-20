{-# LANGUAGE OverloadedStrings #-}

-- | Superdirt-specific implementation
module Minipat.Dirt.Impl
  ( DirtEnv (..)
  , defaultDirtEnv
  , DirtData
  , DirtSt
  , dirtImpl
  , handshake
  )
where

import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (async, cancel, waitCatch)
import Control.Exception (SomeException, bracket, throwIO)
import Dahdit.Midi.Osc (Datum (..), Msg (..), Packet (..))
import Dahdit.Midi.OscAddr (RawAddrPat)
import Dahdit.Network (Conn (..), HostPort (..), resolveAddr, runDecoder, runEncoder, udpServerConn)
import Data.Either (isRight)
import Data.Foldable (foldl')
import Data.Sequence (Seq (..))
import Minipat.Live.Attrs (Attrs, attrsToList)
import Minipat.Live.Core (Env (..), Impl (..), St (..), setPlaying, withData)
import Minipat.Live.Logger (LogAction, logError, logInfo)
import Minipat.Live.Resources (RelVar, Timed (..), relVarAcquire)
import Nanotime (TimeDelta, threadDelayDelta, timeDeltaFromFracSecs)
import Network.Socket qualified as NS

data DirtEnv = DirtEnv
  { deTargetHp :: !HostPort
  , deListenHp :: !HostPort
  , deOscTimeout :: !TimeDelta
  }
  deriving stock (Eq, Ord, Show)

defaultDirtEnv :: DirtEnv
defaultDirtEnv =
  DirtEnv
    { deTargetHp = HostPort (Just "127.0.0.1") 57120
    , deListenHp = HostPort (Just "127.0.0.1") 57129
    , deOscTimeout = timeDeltaFromFracSecs @Double 0.1
    }

data OscConn = OscConn
  { ocTargetAddr :: !NS.SockAddr
  , ocListenConn :: !(Conn NS.SockAddr)
  }

type DirtData = OscConn

type DirtSt = St DirtEnv DirtData

dirtInit :: LogAction -> RelVar -> DirtEnv -> IO DirtData
dirtInit _ rv (DirtEnv targetHp listenHp _) = do
  targetAddr <- resolveAddr targetHp
  relVarAcquire rv $ do
    conn <- udpServerConn Nothing listenHp
    pure (OscConn targetAddr conn)

dirtSend :: LogAction -> ((OscConn -> IO ()) -> IO ()) -> Timed Attrs -> IO ()
dirtSend _ wd = sendPacket' wd . playPacket . timedVal

dirtImpl :: Impl DirtEnv OscConn
dirtImpl = Impl dirtInit dirtSend

sendPacket' :: ((OscConn -> IO ()) -> IO ()) -> Packet -> IO ()
sendPacket' wd packet = wd $ \(OscConn targetAddr (Conn _ enc)) ->
  runEncoder enc targetAddr packet

sendPacket :: DirtSt -> Packet -> IO ()
sendPacket = sendPacket' . withData

withTimeout :: TimeDelta -> IO a -> IO (Either SomeException a)
withTimeout td act = do
  thread <- async act
  _ <- forkFinally (threadDelayDelta td) (const (cancel thread))
  waitCatch thread

recvPacket :: DirtSt -> IO (Either SomeException Packet)
recvPacket st = withData st $ \(OscConn _ (Conn dec _)) ->
  withTimeout (deOscTimeout (envImpl (stEnv st))) $
    runDecoder dec >>= either throwIO pure . snd

-- | Handshake with SuperDirt
-- On success set playing true; on error false
handshake :: DirtSt -> IO ()
handshake st = bracket acq rel (const (pure ()))
 where
  logger = stLogger st
  acq = do
    logInfo logger "Handshaking ..."
    sendPacket st handshakePacket
    recvPacket st
  rel resp = do
    let ok = isRight resp
    if ok
      then logInfo logger "... handshake succeeded"
      else logError logger "... handshake FAILED"
    setPlaying st ok

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
