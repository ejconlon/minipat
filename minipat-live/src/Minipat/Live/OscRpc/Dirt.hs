{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Minipat.Live.OscRpc.Dirt where

import Dahdit.Midi.OscAddr (RawAddrPat (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Minipat.Live.Attrs (Attrs)
import Minipat.Live.EnumString (EnumString (..), deriveEnumString)
import Minipat.Live.OscRpc (RpcCmd (..), RpcType (..))

-- * Dirt impl

deriveEnumString "DiType" ["handshake", "play"]

instance RpcType DiType where
  rtAddr t = RawAddrPat ("/dirt/" <> T.pack (fromEnumString t))

  rtReqTypes = \case
    DiTypeHandshake -> Map.empty
    DiTypePlay -> Map.empty

  rtRepTypes = \case
    DiTypeHandshake -> Just Map.empty
    DiTypePlay -> Nothing

data DiCmd r where
  DiCmdHandshake :: DiCmd ()
  DiCmdPlay :: Attrs -> DiCmd ()

deriving stock instance Eq (DiCmd r)

deriving stock instance Ord (DiCmd r)

deriving stock instance Show (DiCmd r)

instance RpcCmd DiType DiCmd where
  rcType = \case
    DiCmdHandshake {} -> DiTypeHandshake
    DiCmdPlay {} -> DiTypePlay

  rcMkReq = \case
    DiCmdHandshake -> mempty
    DiCmdPlay at -> at

  rcParseRep = \case
    DiCmdHandshake -> Left ()
    DiCmdPlay _ -> Left ()
