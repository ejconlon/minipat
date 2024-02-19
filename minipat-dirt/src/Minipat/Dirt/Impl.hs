{-# LANGUAGE OverloadedStrings #-}

-- | Superdirt-specific implementation
module Minipat.Dirt.Impl
  ( DirtEnv (..)
  , defaultDirtEnv
  , DirtSt
  , dirtImpl
  , handshake
  )
where

import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Concurrent.STM.TVar (readTVar, readTVarIO)
import Control.Exception (SomeException, bracket, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Dahdit.Midi.Osc (Datum (..), Msg (..), Packet (..))
import Dahdit.Midi.OscAddr (RawAddrPat)
import Dahdit.Network (Conn (..), HostPort (..), resolveAddr, runDecoder, runEncoder, udpServerConn)
import Data.Acquire (Acquire)
import Data.Either (isRight)
import Data.Foldable (foldl', for_)
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Sequence (Seq (..))
import Data.Text qualified as T
import Minipat.Live.Attrs (Attrs, attrsToList)
import Minipat.Live.Core (Domain (..), Env (..), Impl (..), St (..), advanceCycleSTM, logEvents, setPlaying, withData)
import Minipat.Live.Logger (LogAction, logError, logInfo)
import Minipat.Live.Osc (PlayEnv (..), PlayErr, convertTape)
import Minipat.Live.Resources (Timed (..), acquireAwait, acquireLoop, relVarAcquire)
import Minipat.Stream (streamRun)
import Minipat.Time (Arc (..), CycleTime (..))
import Nanotime (PosixTime, TimeDelta, addTime, threadDelayDelta, timeDeltaFromFracSecs)
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

acqConn :: DirtEnv -> Acquire OscConn
acqConn (DirtEnv targetHp listenHp _) = do
  targetAddr <- liftIO (resolveAddr targetHp)
  conn <- udpServerConn Nothing listenHp
  pure (OscConn targetAddr conn)

type DirtSt = St DirtEnv OscConn Packet

dirtImpl :: Impl DirtEnv OscConn Packet
dirtImpl =
  Impl
    { implSpawn = \logger dom rv de -> do
        conn <- relVarAcquire rv (acqConn de)
        genTask <- relVarAcquire rv (acqGenTask logger dom)
        sendTask <- relVarAcquire rv (acqSendTask conn dom)
        let tasks = Map.fromList [("gen", genTask), ("send", sendTask)]
        pure (tasks, conn)
    }

genEventsSTM :: Domain Packet -> PosixTime -> STM (PlayEnv, Either PlayErr (Seq (Timed Attrs)))
genEventsSTM dom now = do
  ahead <- readTVar (domAhead dom)
  cps <- readTVar (domCps dom)
  gpc <- readTVar (domGpc dom)
  gcyc <- readTVar (domGenCycle dom)
  let start = CycleTime (gcyc % gpc)
      end = CycleTime ((gcyc + 1) % gpc)
      arc = Arc start end
  stream <- readTVar (domStream dom)
  let tape = streamRun stream arc
      origin = addTime now ahead
      penv = PlayEnv origin start cps
      mpevs = convertTape penv tape
  pure (penv, mpevs)

doGen :: LogAction -> Domain Packet -> PosixTime -> IO ()
doGen logger dom now = do
  mr <- atomically $ do
    playing <- readTVar (domPlaying dom)
    if playing
      then fmap Just (genEventsSTM dom now)
      else pure Nothing
  case mr of
    Nothing -> pure ()
    Just (_, mpevs) ->
      case mpevs of
        Left err -> logError logger (T.pack ("Gen failed: " ++ show err))
        Right pevs -> do
          debug <- readTVarIO (domDebug dom)
          when debug (logEvents logger dom pevs)
          atomically $ do
            advanceCycleSTM dom
            for_ pevs (writeTQueue (domQueue dom) . fmap playPacket)

acqGenTask :: LogAction -> Domain Packet -> Acquire (Async ())
acqGenTask logger dom = acquireLoop (domAhead dom) (doGen logger dom)

sendPacket :: OscConn -> Packet -> IO ()
sendPacket (OscConn targetAddr (Conn _ enc)) = runEncoder enc targetAddr

doSend :: OscConn -> Timed Packet -> IO ()
doSend conn = sendPacket conn . timedVal

acqSendTask :: OscConn -> Domain Packet -> Acquire (Async ())
acqSendTask conn dom = acquireAwait (domPlaying dom) (domQueue dom) (doSend conn)

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
    withData st (`sendPacket` handshakePacket)
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
