{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Prelude where

import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (async, cancel, waitCatch)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Exception (SomeException, bracket, throwIO)
import Control.Monad.IO.Class (liftIO)
import Dahdit.Midi.Osc (Datum (..), Packet)
import Dahdit.Network (Conn (..), HostPort (..), runDecoder, runEncoder, udpServerConn)
import Data.Acquire (Acquire)
import Data.IORef (IORef, newIORef)
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Minipat.Base qualified as B
import Minipat.Dirt.Osc qualified as O
import Minipat.Dirt.Ref (Ref, ReleaseVar)
import Minipat.Dirt.Ref qualified as R
import Minipat.Time qualified as T
import Nanotime (PosixTime, TimeDelta, currentTime, threadDelayDelta, timeDeltaFromFracSecs)
import Network.Socket qualified as NS

data Env = Env
  { envTargetHp :: !HostPort
  , envListenHp :: !HostPort
  , envCps :: !Rational
  , envOscTimeout :: !TimeDelta
  }
  deriving stock (Eq, Ord, Show)

defaultEnv :: Env
defaultEnv =
  Env
    { envTargetHp = HostPort (Just "127.0.0.1") 57120
    , envListenHp = HostPort (Just "127.0.0.1") 57129
    , envCps = 1 % 2 -- 120 bpm, 4 bpc
    , envOscTimeout = timeDeltaFromFracSecs @Double 0.1
    }

data Clock = Clock
  { clDawn :: !PosixTime
  , clCps :: !Rational
  }
  deriving stock (Eq, Ord, Show)

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

withSt :: (St -> IO a) -> IO a
withSt = bracket (initSt defaultEnv) cleanupSt

sendPkt :: St -> Packet -> IO ()
sendPkt (St _ _ _ ref) pkt = R.refUse ref $ \case
  Nothing -> error "Not connected"
  Just (OscConn targetAddr (Conn _ enc)) -> do
    runEncoder enc targetAddr pkt

withTimeout :: TimeDelta -> IO a -> IO (Either SomeException a)
withTimeout td act = do
  thread <- async act
  _ <- forkFinally (threadDelayDelta td) (const (cancel thread))
  waitCatch thread

recvPkt :: St -> IO (Either SomeException Packet)
recvPkt (St (Env _ _ _ timeout) _ _ ref) = R.refUse ref $ \case
  Nothing -> error "Not connected"
  Just (OscConn _ (Conn dec _)) ->
    withTimeout timeout $
      runDecoder dec >>= either throwIO pure . snd

sendHandshake :: St -> IO ()
sendHandshake st = sendPkt st O.handshakePkt

sendPlay :: St -> PosixTime -> Rational -> B.Tape O.OscMap -> IO Bool
sendPlay st dawn cps tape =
  case O.playPkt dawn cps tape of
    Left err -> throwIO err
    Right Nothing -> pure False
    Right (Just pkt) -> True <$ sendPkt st pkt

testHandshake :: IO ()
testHandshake = do
  putStrLn "handshake - initializing"
  withSt $ \st -> do
    putStrLn "sending handshake"
    sendHandshake st
    putStrLn "listening"
    resp <- recvPkt st
    putStrLn "received"
    print resp

testPlay :: IO ()
testPlay = do
  putStrLn "play - initializing"
  withSt $ \st -> do
    dawn <- currentTime
    putStrLn ("sending play @ " <> show dawn)
    let cps = 1 % 2
    _ <-
      sendPlay st dawn cps $
        B.tapeSingleton $
          B.Ev (T.Span (T.Arc 0 1) (Just (T.Arc 0 1))) $
            Map.fromList
              [ ("sound", DatumString "tabla")
              , ("orbit", DatumInt32 0)
              ]
    putStrLn "done"

testLoop :: IO ()
testLoop = do
  tdv <- newTVarIO (timeDeltaFromFracSecs @Double 0.5)
  withSt $ \st -> do
    _ <- R.refLoop (stRel st) tdv $ do
      putStrLn "hello"
      pure Nothing
    threadDelayDelta (timeDeltaFromFracSecs @Double 2)
