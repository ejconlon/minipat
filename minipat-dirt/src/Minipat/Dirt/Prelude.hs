module Minipat.Dirt.Prelude where

import Dahdit.Network (Conn, HostPort (..), udpClientConn)
import Network.Socket qualified as NS
import Data.Acquire (Acquire)
import Minipat.Dirt.Ref (ReleaseVar, Ref)
import Minipat.Dirt.Ref qualified as R

-- private con
data Dirt = Dirt !NS.SockAddr !(Conn ())

newtype Env = Env
  { envDirtHp :: HostPort
  } deriving stock (Eq, Ord, Show)

defaultEnv :: Env
defaultEnv = Env
  { envDirtHp = HostPort (Just "127.0.0.1") 57120
  }

data St = St
  { stEnv :: !Env
  , stRv :: !ReleaseVar
  , stDirt :: !(Ref Dirt)
  }

-- private
acqDirt :: HostPort -> Acquire Dirt
acqDirt = fmap (uncurry Dirt) . udpClientConn Nothing

initSt :: Env -> IO St
initSt env = do
  rv <- R.releaseVarCreate
  ref <- R.refCreate rv (acqDirt (envDirtHp env))
  pure (St env rv ref)

reinitSt :: St -> IO ()
reinitSt st = R.refReplace (stDirt st) (acqDirt (envDirtHp (stEnv st)))

