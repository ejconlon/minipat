{-# LANGUAGE OverloadedStrings #-}

module Minipat.Live.OscRpc where

import Control.Exception (Exception)
import Dahdit.Midi.Osc (Datum (..), DatumType (..), Msg (..), Packet (..))
import Dahdit.Midi.OscAddr (RawAddrPat)
import Data.Foldable (foldl')
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Minipat.Live.Attrs (Attrs, IsAttrs (..), attrsSingleton, attrsToList)
import Minipat.Live.Convert (ConvErr, ConvM)
import Minipat.Live.EnumString (EnumString, allEnumStrings)

-- * General classes and data types

data Required
  = RequiredYes
  | RequiredNo
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data AttrType
  = AttrTypeDatum !DatumType
  | AttrTypeEnum !(Seq Text)
  deriving stock (Eq, Ord, Show)

atEnum :: (Bounded a, EnumString a) => Proxy a -> AttrType
atEnum = AttrTypeEnum . Seq.fromList . fmap T.pack . allEnumStrings

type AttrTypes = Map Text (Required, AttrType)

class RpcType t where
  rtAddr :: t -> RawAddrPat
  rtReqTypes :: t -> AttrTypes
  rtRepTypes :: t -> AttrTypes

class (RpcType t) => RpcCmd t c | c -> t where
  rcType :: c r -> t
  rcMkReq :: c r -> Attrs
  rcParseRep :: c r -> Either r (ConvM r)

-- * RPC impl

newtype RemoteErr = RemoteErr {unRemoteErr :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Exception RemoteErr

data RpcErr
  = RpcErrRemote !RemoteErr
  | RpcErrConv !ConvErr
  deriving stock (Eq, Ord, Show)

instance Exception RpcErr

newtype RequestId = RequestId {unRequestId :: Int32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum)

instance IsAttrs RequestId where
  toAttrs (RequestId x) = attrsSingleton "!requestId" (DatumInt32 x)

namedPayload :: Attrs -> Seq Datum
namedPayload = foldl' go Empty . attrsToList
 where
  go !acc (k, v) = acc :|> DatumString k :|> v

mkCmdReq :: (RpcCmd t c) => RequestId -> c r -> Packet
mkCmdReq rid cmd =
  let ty = rcType cmd
      addr = rtAddr ty
      args = rcMkReq cmd
      attrs = toAttrs rid <> args
  in  PacketMsg (Msg addr (namedPayload attrs))

-- rpc :: RpcCmd c => (Value -> IO Value) -> TVar RequestId -> c r -> IO (Either RpcErr r)
-- rpc act rv cmd = do
--   let ep = mkRpcCmdEndpoint cmd
--       args = mkRpcCmdArgs cmd
--   rid <- atomically (stateTVar rv (\s -> (s, succ s)))
--   let wreq = WireReq rid ep args
--       vreq = mkReqValue wreq
--   vrep <- act vreq
--   case runP parseRepValue vrep of
--     Left err -> pure (Left (RpcErrParse err))
--     Right wrep ->
