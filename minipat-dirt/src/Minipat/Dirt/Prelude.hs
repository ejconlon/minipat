module Minipat.Dirt.Prelude where

import Data.ByteString.Short (ShortByteString)
import Dahdit (Get, get, getRemainingString)
import Control.Exception (throwIO, SomeException, bracket)
import Control.Concurrent (forkFinally)
import Control.Monad.IO.Class (liftIO)
import Data.Ratio ((%))
import Data.IORef (IORef, newIORef)
import Dahdit.Network (Conn (..), HostPort (..), udpServerConn, runEncoder, Decoder (..))
import Network.Socket qualified as NS
import Data.Acquire (Acquire)
import Dahdit.Midi.Osc (Packet)
import Minipat.Dirt.Ref (ReleaseVar, Ref)
import Minipat.Dirt.Ref qualified as R
import Minipat.Dirt.Osc qualified as O
import Nanotime (PosixTime, TimeDelta, currentTime, timeDeltaFromFracSecs, threadDelayDelta)
import Control.Concurrent.Async (async, waitCatch, cancel)

data Env = Env
  { envTargetHp :: !HostPort
  , envListenHp :: !HostPort
  , envCps :: !Rational
  , envOscTimeout :: !TimeDelta
  } deriving stock (Eq, Ord, Show)

defaultEnv :: Env
defaultEnv = Env
  { envTargetHp = HostPort (Just "127.0.0.1") 57120
  , envListenHp = HostPort (Just "127.0.0.1") 57129
  , envCps = 1 % 2 -- 120 bpm, 4 bpc
  , envOscTimeout = timeDeltaFromFracSecs @Double 0.1
  }

data Clock = Clock
  { clDawn :: !PosixTime
  , clCps :: !Rational
  } deriving stock (Eq, Ord, Show)

data OscConn = OscConn
  { ocTargetAddr :: !NS.SockAddr
  , ocListenConn :: !(Conn NS.SockAddr)
  }

data St = St
  { stEnv :: !Env
  , stRel :: !ReleaseVar
  , stClock :: !(IORef Clock)
  , stConn :: !(Ref OscConn)
  }

-- TODO export this from dahdit-network
resolveAddr :: HostPort -> IO NS.SockAddr
resolveAddr hp@(HostPort host port) = do
  infos <- NS.getAddrInfo Nothing host (Just (show port))
  case infos of
    [] -> fail ("Could not resolve address: " ++ show hp)
    info : _ -> pure (NS.addrAddress info)

acqConn :: Env -> Acquire OscConn
acqConn (Env targetHp listenHp _ _) = do
  targetAddr <- liftIO (resolveAddr targetHp)
  conn <- udpServerConn Nothing listenHp
  pure (OscConn targetAddr conn)

initSt :: Env -> IO St
initSt env = do
  rv <- R.releaseVarCreate
  ref <- R.refCreate rv (acqConn env)
  now <- currentTime
  cv <- newIORef (Clock now (envCps env))
  pure (St env rv cv ref)

reinitSt :: St -> IO ()
reinitSt st = R.refReplace (stConn st) (acqConn (stEnv st))

cleanupSt :: St -> IO ()
cleanupSt = R.releaseVarCleanup . stRel

sendPkt :: St -> Packet -> IO ()
sendPkt (St _ _ _ ref) pkt = R.refUse ref $ \case
  Nothing -> error "Not connected"
  Just (OscConn targetAddr (Conn _ enc)) -> do
    runEncoder enc targetAddr pkt

-- type Recv = ShortByteString
-- getRecv :: Get Recv
-- getRecv = getRemainingString

type Recv = Packet
getRecv :: Get Recv
getRecv = get

withTimeout :: TimeDelta -> IO a -> IO (Either SomeException a)
withTimeout td act = do
  thread <- async act
  _ <- forkFinally (threadDelayDelta td) (const (cancel thread))
  waitCatch thread

recvPkt :: St -> IO (Either SomeException Recv)
recvPkt (St (Env _ _ _ timeout) _ _ ref) = R.refUse ref $ \case
  Nothing -> error "Not connected"
  Just (OscConn _ (Conn dec _)) ->
    withTimeout timeout $
      unDecoder dec getRecv >>= either throwIO pure . snd

sendHandshake :: St -> IO ()
sendHandshake st = sendPkt st O.handshakePkt

test :: IO ()
test = do
  putStrLn "initializing"
  bracket (initSt defaultEnv) cleanupSt $ \st -> do
    putStrLn "sending handshake"
    sendHandshake st
    putStrLn "listening"
    resp <- recvPkt st
    putStrLn "received"
    print resp
