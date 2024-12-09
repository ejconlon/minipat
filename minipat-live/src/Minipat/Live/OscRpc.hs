{-# LANGUAGE OverloadedStrings #-}

-- |
--  - We define an RPC-over-OSC protocol (like the one in SuperDirt):
--  -
--  - * Messages can be one-shot or request-response
--  - * Replies should be send to the original OSC address with the
--  -  additional suffix `/reply`
--  -    * Example: `/dirt/handshake` to `/dirt/handshake/reply`
--  - * Messages contain lists of key-value pairs (string datum, then
--  -  any kind of datum, repeating).
--  - * Attribute names starting with `!` are system level attributes and
--  -  should be removed before further processing.
--  -    * `!requestId` is one such attribute that should be carried
--  -     over into a responses.
--  -   * Errors can be signaled by the attribute `!error` mapping to a string
--  -    datum containing a reason.
--  - * Type checking of requests and responses should be lenient -
--  -  it's OK to have unrecognized attributes.
--  - * Responses should carry the corresponding `!requestId`, but if they
--  -  do not, they should be associated with the last request to the original
--  -  address.
--  -    * Clients may enforce that reply addresses are also correct.
--  -    * `!requestId` values should be distinct.
--  - * It is expected that requests or replies may be lost or reordered, so
--  -  plan accordingly.
module Minipat.Live.OscRpc where

import Control.Concurrent.STM.TMVar (TMVar)
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Exception (Exception)
import Dahdit.Midi.Osc (Datum (..), DatumType (..), Msg (..))
import Dahdit.Midi.OscAddr (RawAddrPat)
import Data.Foldable (foldl')
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Minipat.Live.Attrs (Attrs, ToAttrs (..), attrsSingleton, attrsToList)
import Minipat.Live.Convert (ConvErr, ConvM, runConvM)
import Minipat.Live.EnumString (EnumString, allEnumStrings)
import Nanotime (PosixTime, TimeDelta)

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
  rtRepTypes :: t -> Maybe AttrTypes

class (RpcType t) => RpcCmd t c | c -> t where
  rcType :: c r -> t
  rcMkReq :: c r -> Attrs
  rcParseRep :: c r -> Either r (ConvM r)

-- * RPC impl

newtype RequestId = RequestId {unRequestId :: Int32}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num, Enum)

instance ToAttrs RequestId where
  toAttrs (RequestId x) = attrsSingleton "!requestId" (DatumInt32 x)

lookupRequestId :: Attrs -> Maybe RequestId
lookupRequestId = error "TODO"

newtype RemoteErr = RemoteErr {unRemoteErr :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Exception RemoteErr

instance ToAttrs RemoteErr where
  toAttrs (RemoteErr x) = attrsSingleton "!error" (DatumString x)

lookupRemoteErr :: Attrs -> Maybe RemoteErr
lookupRemoteErr = undefined

data RpcErr
  = -- | Remote side signaled an error
    RpcErrRemote !RemoteErr
  | -- | Response parsing failed
    RpcErrConv !ConvErr
  | -- | No match for reply with id and address
    RpcErrUnmatchedRep !RequestId !RawAddrPat
  | -- | Mismatch of reply (args: rid, actual, expected)
    RpcErrAddrMismatch !RequestId !RawAddrPat !RawAddrPat
  | -- | Timeout waiting for reply
    RpcErrTimeoutRep !RequestId !RawAddrPat !PosixTime
  deriving stock (Eq, Ord, Show)

instance Exception RpcErr

namedPayload :: Attrs -> Seq Datum
namedPayload = foldl' go Empty . attrsToList
 where
  go !acc (k, v) = acc :|> DatumString k :|> v

mkCmdReq :: (RpcCmd t c) => RequestId -> c r -> Msg
mkCmdReq rid cmd =
  let ty = rcType cmd
      addr = rtAddr ty
      args = rcMkReq cmd
      attrs = toAttrs rid <> args
  in  Msg addr (namedPayload attrs)

parseCmdRep :: (RpcCmd t c) => c r -> Attrs -> Either ConvErr r
parseCmdRep cmd at = case rcParseRep cmd of
  Left r -> Right r
  Right p -> runConvM p at

-- * Implementation

type WaitVar r = TMVar (Either RpcErr r)

data Waiter c where
  Waiter :: RequestId -> c r -> RawAddrPat -> PosixTime -> WaitVar r -> Waiter c

waiterMatches :: Msg -> Waiter c -> Bool
waiterMatches m w = error "TODO"

waiterExpired :: PosixTime -> Waiter c -> Bool
waiterExpired t w = error "TODO"

waiterRun :: Msg -> Waiter c -> IO (Either RpcErr ())
waiterRun m w = error "TODO"

data OscProtoEnv c = OscProtoEnv
  { opeTimeout :: !TimeDelta
  , opeIdSource :: !(TVar RequestId)
  , opeWaiters :: !(TVar (Seq (Waiter c)))
  }

newOscProtoEnvIO :: TimeDelta -> RequestId -> IO (OscProtoEnv c)
newOscProtoEnvIO to rid = OscProtoEnv to <$> newTVarIO rid <*> newTVarIO Empty

expireWaiters :: OscProtoEnv c -> PosixTime -> IO ()
expireWaiters (OscProtoEnv _to _ _wes) _now = error "TODO"

handleRecvMsg :: OscProtoEnv c -> Msg -> IO (Either RpcErr a)
handleRecvMsg = error "TODO"

sendMsgWith :: (RpcCmd t c) => (Msg -> IO ()) -> OscProtoEnv c -> c r -> IO (WaitVar r)
sendMsgWith _send _ope _cmd = error "TODO"

-- TODO finish implementing

-- data OscTaskEnv c a = OscTaskEnv
--   { oteSend :: !(Msg -> IO ())
--   , oteRecv :: !(IO Msg)
--   , oteProtoEnv :: !(OscProtoEnv c a)
--   , oteExpireTask :: !(Async ())
--   , oteRecvTask :: !(Async ())
--   }
--
-- newOscTaskEnv :: (Msg -> IO ()) -> IO Msg -> OscProtoEnv c a -> OscTaskEnv c a
-- newOscTaskEnv = error "TODO"
--
-- sendMsg :: OscTaskEnv c a -> c r -> IO (WaitVar r)
-- sendMsg = undefined
