module Minipat.Dirt.Prelude where

import Data.Ratio ((%))
import Data.IORef (IORef, newIORef)
import Dahdit.Network (Conn, HostPort (..), udpClientConn)
import Network.Socket qualified as NS
import Data.Acquire (Acquire)
import Minipat.Dirt.Ref (ReleaseVar, Ref)
import Minipat.Dirt.Ref qualified as R
import Nanotime (PosixTime, currentTime)

-- private con
data Dirt = Dirt !NS.SockAddr !(Conn ())

data Env = Env
  { envDirtHp :: !HostPort
  , envCps :: !Rational
  } deriving stock (Eq, Ord, Show)

defaultEnv :: Env
defaultEnv = Env
  { envDirtHp = HostPort (Just "127.0.0.1") 57120
  , envCps = 1 % 2 -- 120 bpm, 4 bpc
  }

data Clock = Clock
  { clDawn :: !PosixTime
  , clCps :: !Rational
  } deriving stock (Eq, Ord, Show)

data St = St
  { stEnv :: !Env
  , stRel :: !ReleaseVar
  , stClock :: !(IORef Clock)
  , stDirt :: !(Ref Dirt)
  }

-- private
acqDirt :: HostPort -> Acquire Dirt
acqDirt = fmap (uncurry Dirt) . udpClientConn Nothing

initSt :: Env -> IO St
initSt env = do
  rv <- R.releaseVarCreate
  ref <- R.refCreate rv (acqDirt (envDirtHp env))
  now <- currentTime
  cv <- newIORef (Clock now (envCps env))
  pure (St env rv cv ref)

reinitSt :: St -> IO ()
reinitSt st = R.refReplace (stDirt st) (acqDirt (envDirtHp (stEnv st)))

cleanupSt :: St -> IO ()
cleanupSt = R.releaseVarCleanup . stRel

