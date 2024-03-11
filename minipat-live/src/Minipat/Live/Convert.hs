-- | Utilities to convert 'Attrs' to more useful types.
module Minipat.Live.Convert
  ( ConvErr (..)
  , ConvM
  , runConvM
  , lookupM
  , defaultM
  , getM
  , Branch (..)
  , branchM
  , branchM'
  , optBranchM
  , optBranchM'
  , finiteM
  , finiteM'
  , optFiniteM
  , optFiniteM'
  , enumM
  , optEnumM
  , restM
  )
where

import Control.Exception (Exception)
import Control.Monad.Except (Except, MonadError (..), runExcept, tryError)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State (MonadState (..), StateT (..), modify')
import Dahdit.Midi.Osc (Datum, DatumType)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Minipat.Live.Attrs (Attrs (..), attrsLookup)
import Minipat.Live.Datum (DatumProxy (..), DatumTypeErr (..), castDatum, datumProxyType)
import Minipat.Live.EnumString (EnumString (..))

data ConvErr
  = ConvErrFail !Text
  | ConvErrMissing !Text
  | ConvErrMissingAll !(Set Text)
  | ConvErrExclusive !(Set Text) !Text !Text
  | ConvErrMatch !Text !DatumType !DatumType
  | ConvErrFinite !Text !DatumType
  | ConvErrEnum !Text !Text
  deriving stock (Eq, Ord, Show)

instance Exception ConvErr

newtype ConvM a = ConvM {unConvM :: ReaderT Attrs (StateT (Set Text) (Except ConvErr)) a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadFail ConvM where
  fail = ConvM . throwError . ConvErrFail . T.pack

runConvM :: ConvM a -> Attrs -> Either ConvErr a
runConvM m at = fmap fst (implRunConvM m at Set.empty)

implRunConvM :: ConvM a -> Attrs -> Set Text -> Either ConvErr (a, Set Text)
implRunConvM m at s = runExcept (runStateT (runReaderT (unConvM m) at) s)

implLookupM :: Text -> ConvM (Maybe Datum)
implLookupM k = ConvM $ do
  mv <- asks (attrsLookup k)
  case mv of
    Nothing -> pure ()
    Just _ -> modify' (Set.insert k)
  pure mv

implTryErrorM :: ConvM a -> ConvM (Either ConvErr a)
implTryErrorM m = ConvM (tryError (unConvM m))

lookupM :: Text -> DatumProxy a -> ConvM (Maybe a)
lookupM k p = implLookupM k >>= traverse (matchM k p)

defaultM :: Text -> DatumProxy a -> a -> ConvM a
defaultM k p d = implLookupM k >>= maybe (pure d) (matchM k p)

getM :: Text -> DatumProxy a -> ConvM a
getM k p = lookupM k p >>= maybe (ConvM (throwError (ConvErrMissing k))) pure

matchM :: Text -> DatumProxy a -> Datum -> ConvM a
matchM k p v = case castDatum p v of
  Left (DatumTypeErr expected actual) -> ConvM (throwError (ConvErrMatch k expected actual))
  Right val -> pure val

exclusiveM :: (Foldable f) => f Text -> ConvM (Text, Datum)
exclusiveM = exclusiveM' . Set.fromList . toList

exclusiveM' :: Set Text -> ConvM (Text, Datum)
exclusiveM' ks0 = go Nothing (toList ks0)
 where
  go !racc = \case
    [] -> case racc of
      Nothing -> ConvM (throwError (ConvErrMissingAll ks0))
      Just kv@(k, _) -> kv <$ ConvM (modify' (Set.insert k))
    k : ks -> do
      mv <- ConvM (asks (attrsLookup k))
      case mv of
        Nothing -> go racc ks
        Just v -> case racc of
          Nothing -> go (Just (k, v)) ks
          Just (k', _) -> ConvM (throwError (ConvErrExclusive ks0 k' k))

data Branch a where
  Branch :: DatumProxy z -> (z -> ConvM a) -> Branch a

runBranch :: Branch a -> Text -> Datum -> ConvM a
runBranch (Branch p f) k v = matchM k p v >>= f

branchM :: (Foldable f) => f (Text, Branch a) -> ConvM a
branchM = branchM' . Map.fromList . toList

branchM' :: Map Text (Branch a) -> ConvM a
branchM' kfs = do
  (k, v) <- exclusiveM' (Map.keysSet kfs)
  runBranch (kfs Map.! k) k v

optBranchM :: (Foldable f) => f (Text, Branch a) -> ConvM (Maybe a)
optBranchM = optBranchM' . Map.fromList . toList

optBranchM' :: Map Text (Branch a) -> ConvM (Maybe a)
optBranchM' kfs = do
  ea <- implTryErrorM (exclusiveM' (Map.keysSet kfs))
  mkv <- case ea of
    Left (ConvErrExclusive {}) -> pure Nothing
    Left err -> ConvM (throwError err)
    Right kv -> pure (Just kv)
  case mkv of
    Nothing -> pure Nothing
    Just (k, v) -> fmap Just (runBranch (kfs Map.! k) k v)

restM :: ConvM Attrs
restM = ConvM (liftA2 (\at ks -> Attrs (Map.withoutKeys (unAttrs at) ks)) ask get)

implFiniteM :: (Ord z) => Text -> DatumProxy z -> Map z (ConvM a) -> z -> ConvM a
implFiniteM k p vms z = case Map.lookup z vms of
  Nothing -> ConvM (throwError (ConvErrFinite k (datumProxyType p)))
  Just ma -> ma

finiteM :: (Foldable f, Ord z) => Text -> DatumProxy z -> f (z, ConvM a) -> ConvM a
finiteM k p = finiteM' k p . Map.fromList . toList

finiteM' :: (Ord z) => Text -> DatumProxy z -> Map z (ConvM a) -> ConvM a
finiteM' k p vms = getM k p >>= implFiniteM k p vms

optFiniteM :: (Foldable f, Ord z) => Text -> DatumProxy z -> f (z, ConvM a) -> ConvM (Maybe a)
optFiniteM k p = optFiniteM' k p . Map.fromList . toList

optFiniteM' :: (Ord z) => Text -> DatumProxy z -> Map z (ConvM a) -> ConvM (Maybe a)
optFiniteM' k p vms =
  lookupM k p >>= \case
    Nothing -> pure Nothing
    Just v -> fmap Just (implFiniteM k p vms v)

implEnumM :: (EnumString z) => Text -> Text -> ConvM z
implEnumM k v = case toEnumString (T.unpack v) of
  Nothing -> ConvM (throwError (ConvErrEnum k v))
  Just z -> pure z

enumM :: (EnumString z) => Text -> ConvM z
enumM k = getM k DatumProxyString >>= implEnumM k

optEnumM :: (EnumString z) => Text -> ConvM (Maybe z)
optEnumM k =
  lookupM k DatumProxyString >>= \case
    Nothing -> pure Nothing
    Just v -> fmap Just (implEnumM k v)
