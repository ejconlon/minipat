{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Dirt where

import Control.Monad (unless)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.State.Strict (MonadState (..), StateT, evalStateT)
import Dahdit (ShortByteString)
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Dahdit.Midi.Osc (Datum (..), DatumType (..), Msg (..), PortMsg, datumType)
import Dahdit.Midi.OscAddr (RawAddrPat)
import Nanotime (NtpTime)
import Optics (AffineTraversal', Prism', gafield, gconstructor, ix, preview, prism', review, set, (%))

-- TODO move into source lib
deriving stock instance Generic Datum

data ArgsErr
  = ArgsErrEmpty
  | ArgsErrMismatch !Datum !Datum
  | ArgsErrTyMismatch !DatumType !DatumType
  | ArgsErrLeftover !Int
  | ArgsErrInvalidField !Text !Datum
  | ArgsErrMissingFields !(Set Text)
  deriving stock (Eq, Ord, Show)

type P = StateT (Seq Datum) (Except ArgsErr)

rethrow :: Either ArgsErr a -> P a
rethrow = either throwError pure

parseArgs :: P a -> Seq Datum -> Either ArgsErr a
parseArgs m s = runExcept (evalStateT m s)

getArgRaw :: (Datum -> Either ArgsErr a) -> P a
getArgRaw f = do
  args <- get
  case args of
    Empty -> throwError ArgsErrEmpty
    hd :<| tl -> do
      a <- rethrow (f hd)
      put tl
      pure a

getArg :: P Datum
getArg = getArgRaw Right

getArgExact :: Datum -> P ()
getArgExact wantDat = getArgRaw $ \actualDat ->
  if actualDat == wantDat
    then Right ()
    else Left (ArgsErrMismatch actualDat wantDat)

data DatumPrism a = DatumPrism
  { dpType :: !DatumType
  , dpPrism :: !(Prism' Datum a)
  }

asInt32 :: DatumPrism Int32
asInt32 = DatumPrism DatumTypeInt32 (gconstructor @"DatumInt32")

asInt64 :: DatumPrism Int64
asInt64 = DatumPrism DatumTypeInt64 (gconstructor @"DatumInt64")

asFloat :: DatumPrism Float
asFloat = DatumPrism DatumTypeFloat (gconstructor @"DatumFloat")

asDouble :: DatumPrism Double
asDouble = DatumPrism DatumTypeDouble (gconstructor @"DatumDouble")

asString :: DatumPrism Text
asString = DatumPrism DatumTypeString (gconstructor @"DatumString")

asBlob :: DatumPrism ShortByteString
asBlob = DatumPrism DatumTypeBlob (gconstructor @"DatumBlob")

asTime :: DatumPrism NtpTime
asTime = DatumPrism DatumTypeTime (gconstructor @"DatumTime")

asMidi :: DatumPrism PortMsg
asMidi = DatumPrism DatumTypeMidi (gconstructor @"DatumMidi")

viewDatum :: DatumPrism a -> Datum -> Either DatumType a
viewDatum (DatumPrism _ pr) dat =
  case preview pr dat of
    Just val -> Right val
    Nothing -> Left (datumType dat)

previewDatum :: DatumPrism a -> Datum -> Maybe a
previewDatum dr = either (const Nothing) Just . viewDatum dr

reviewDatum :: DatumPrism a -> a -> Datum
reviewDatum (DatumPrism _ pr) = review pr

getArgTyped :: DatumPrism a -> P a
getArgTyped dr = getArgRaw $ \actualDat ->
  case viewDatum dr actualDat of
    Left actualTy -> Left (ArgsErrTyMismatch actualTy (dpType dr))
    Right a -> Right a

foldArgs :: s -> (s -> P s) -> P s
foldArgs start f = go start
 where
  go !val = do
    args <- get
    case args of
      Empty -> pure val
      _ -> do
        val' <- f val
        args' <- get
        unless
          (Seq.length args' < Seq.length args)
          (error "Not consuming args")
        go val'

forArgs :: P a -> P (Seq a)
forArgs act = foldArgs Empty (\s -> fmap (s :|>) act)

endArgs :: P ()
endArgs = do
  args <- get
  case args of
    Empty -> pure ()
    _ -> throwError (ArgsErrLeftover (Seq.length args))

data DatumField b where
  DatumField :: Prism' Datum a -> AffineTraversal' b a -> DatumField b

previewDatumField :: DatumField b -> b -> Maybe Datum
previewDatumField (DatumField x y) b = fmap (review x) (preview y b)

setDatumField :: DatumField b -> b -> Datum -> Maybe b
setDatumField (DatumField pri len) b v = fmap (flip (set len) b) (preview pri v)

setFirstDatumField :: [DatumField b] -> b -> Datum -> Maybe b
setFirstDatumField ss0 b v = foldr go Nothing ss0
 where
  go field = maybe id (const . Just) (setDatumField field b v)

data Struct b = Struct
  { structNull :: !b
  , structRequired :: !(Set Text)
  , structFields :: !(Text -> [DatumField b])
  }

expectStruct :: Struct b -> P b
expectStruct (Struct nul req fields) = do
  (b', ks') <- foldArgs (nul, Set.empty) $ \(b, ks) -> do
    k <- getArgTyped asString
    v <- getArg
    case setFirstDatumField (fields k) b v of
      Nothing -> throwError (ArgsErrInvalidField k v)
      Just b' -> pure (b', Set.insert k ks)
  unless (Set.isSubsetOf ks' req) (throwError (ArgsErrMissingFields (Set.difference req ks')))
  pure b'

data AddrSerde a = AddrSerde
  { addrSerdeTo :: !(a -> RawAddrPat)
  , addrSerdeFrom :: !(RawAddrPat -> Maybe a)
  }

exactAddrSerde :: RawAddrPat -> AddrSerde ()
exactAddrSerde pat = AddrSerde (const pat) (\pat' -> if pat == pat' then Just () else Nothing)

data ArgsSerde b = ArgsSerde
  { argsSerdeTo :: !(b -> Seq Datum)
  , argsSerdeFrom :: !(Seq Datum -> Either ArgsErr b)
  }

mkArgsSerde :: (b -> Seq Datum) -> P b -> ArgsSerde b
mkArgsSerde argsTo argsParser = ArgsSerde argsTo (parseArgs argsParser)

structArgsSerde :: Struct b -> ArgsSerde b
structArgsSerde struct = ArgsSerde to from
 where
  to = error "TODO"
  from = parseArgs (expectStruct struct)

data Serde a b = Serde {serdeAddr :: !(AddrSerde a), serdeArgs :: !(ArgsSerde b)}

serdeTo :: Serde a b -> a -> b -> Msg
serdeTo (Serde (AddrSerde addrTo _) (ArgsSerde argsTo _)) a b = Msg (addrTo a) (argsTo b)

serdeFrom :: Serde a b -> Msg -> Maybe (a, Either ArgsErr b)
serdeFrom (Serde (AddrSerde _ addrFrom) (ArgsSerde _ argsFrom)) (Msg addr args) =
  fmap (,argsFrom args) (addrFrom addr)

data Handshake = Handshake
  deriving stock (Eq, Ord, Show)

handshakeS :: Serde () Handshake
handshakeS = Serde (exactAddrSerde "/dirt/handshake") (mkArgsSerde argsTo argsParser)
 where
  argsTo = const Empty
  argsParser = do
    endArgs
    pure Handshake

data HandshakeReply = HandshakeReply
  { hrServerHostname :: !Text
  , hrServerPort :: !Int32
  , hrControlBusIndices :: !(Seq Int32)
  }
  deriving stock (Eq, Ord, Show)

handshakeReplyS :: Serde () HandshakeReply
handshakeReplyS = Serde (exactAddrSerde "/dirt/handshake/reply") (mkArgsSerde argsTo argsParser)
 where
  argsTo (HandshakeReply host port idxs) = prefix <> fmap DatumInt32 idxs
   where
    prefix =
      Seq.fromList
        [ DatumString "&serverHostname"
        , DatumString host
        , DatumString "&serverPort"
        , DatumInt32 port
        , DatumString "&controlBusIndices"
        ]
  argsParser = do
    getArgExact (DatumString "&serverHostname")
    host <- getArgTyped asString
    getArgExact (DatumString "&serverPort")
    port <- getArgTyped asInt32
    getArgExact (DatumString "&controlBusIndices")
    idxs <- forArgs (getArgTyped asInt32)
    pure (HandshakeReply host port idxs)

data Play = Play
  { playId :: !Text
  , playOrbit :: !Int32
  , playCps :: !Float
  , playCycle :: !Float
  , playDelta :: !Float
  , playOther :: !(Map Text Datum)
  }
  deriving stock (Eq, Ord, Show, Generic)

playStruct :: Struct Play
playStruct = Struct nul req sets
 where
  nul = Play "" 0 0 0 0 Map.empty
  req = Set.fromList ["_id_", "orbit", "cps", "cycle", "delta"]
  sets = \case
    "_id_" -> [DatumField (gconstructor @"DatumString") (gafield @"playId")]
    "orbit" -> [DatumField (gconstructor @"DatumInt32") (gafield @"playOrbit")]
    "cps" -> [DatumField (gconstructor @"DatumFloat") (gafield @"playCps")]
    "cycle" -> [DatumField (gconstructor @"DatumFloat") (gafield @"playCycle")]
    "delta" -> [DatumField (gconstructor @"DatumFloat") (gafield @"playDelta")]
    k -> [DatumField (prism' id Just) (gafield @"playOther" % ix k)]

playS :: Serde () Play
playS = Serde (exactAddrSerde "/dirt/play") (structArgsSerde playStruct)
