{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Minipat.Live.Renoise where

import Dahdit.Midi.Osc (DatumType (..))
import Dahdit.Midi.OscAddr (RawAddrPat (..))
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Minipat.Live.EnumString (EnumString (..), deriveEnumString)
import Minipat.Live.OscRpc (AttrType (..), Required (..), RpcCmd (..), RpcType (..), atEnum)

-- * Renoise impl

deriveEnumString "ShowType" ["message", "error", "warning", "status"]

deriveEnumString "TrackOp" ["mute", "unmute", "solo"]

deriveEnumString "TrackType" ["sequence", "master", "send", "group"]

deriveEnumString "MuteState" ["active", "off", "muted"]

deriveEnumString "ReType" ["handshake", "show", "numTracks", "trackType", "trackOp"]

instance RpcType ReType where
  rtAddr t = RawAddrPat ("/minipat/" <> T.pack (fromEnumString t))

  rtReqTypes = \case
    ReTypeHandshake -> Map.empty
    ReTypeShow ->
      Map.fromList
        [ ("showType", (RequiredYes, atEnum (Proxy @ShowType)))
        , ("message", (RequiredYes, AttrTypeDatum DatumTypeString))
        ]
    ReTypeNumTracks -> Map.empty
    ReTypeTrackType ->
      Map.fromList
        [ ("trackIx", (RequiredYes, AttrTypeDatum DatumTypeInt32))
        ]
    ReTypeTrackOp ->
      Map.fromList
        [ ("trackIx", (RequiredYes, AttrTypeDatum DatumTypeInt32))
        , ("trackOp", (RequiredYes, atEnum (Proxy @TrackOp)))
        ]

  rtRepTypes = \case
    ReTypeHandshake -> Map.empty
    ReTypeShow -> Map.empty
    ReTypeNumTracks ->
      Map.fromList
        [ ("numTracks", (RequiredYes, AttrTypeDatum DatumTypeInt32))
        ]
    ReTypeTrackType ->
      Map.fromList
        [ ("trackType", (RequiredYes, atEnum (Proxy @TrackType)))
        ]
    ReTypeTrackOp -> Map.empty

data ReCmd r where
  ReCmdHandshake :: ReCmd ()
  ReCmdShow :: ShowType -> Text -> ReCmd ()
  ReCmdNumTracks :: ReCmd Int32
  ReCmdTrackType :: Int32 -> ReCmd TrackType
  ReCmdTrackOp :: Int32 -> TrackOp -> ReCmd ()

deriving stock instance Eq (ReCmd r)

deriving stock instance Ord (ReCmd r)

deriving stock instance Show (ReCmd r)

instance RpcCmd ReType ReCmd where
  rcType = \case
    ReCmdHandshake {} -> ReTypeHandshake
    ReCmdShow {} -> ReTypeShow
    ReCmdNumTracks {} -> ReTypeNumTracks
    ReCmdTrackType {} -> ReTypeTrackType
    ReCmdTrackOp {} -> ReTypeTrackOp

  rcMkReq = \case
    ReCmdHandshake -> mempty
    ReCmdShow {} -> undefined
    ReCmdNumTracks {} -> undefined
    ReCmdTrackType {} -> undefined
    ReCmdTrackOp {} -> undefined

  rcParseRep = \case
    ReCmdHandshake -> undefined
    ReCmdShow {} -> undefined
    ReCmdNumTracks {} -> undefined
    ReCmdTrackType {} -> undefined
    ReCmdTrackOp {} -> undefined
