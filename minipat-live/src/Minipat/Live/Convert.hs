module Minipat.Live.Convert where

import Control.Exception (Exception)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Dahdit.Midi.Osc (Datum, DatumType)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Minipat.Live.Attrs (Attrs, attrsLookup, attrsLookupDefault)
import Minipat.Live.Datum (DatumProxy)

data ConvErr
  = ConvErrFail !Text
  | ConvErrMissing !Text
  | ConvErrMissingAll !(Set Text)
  | ConvErrExclusive !(Set Text) !Text !Text
  | ConvErrMatch !DatumType !DatumType
  deriving stock (Eq, Ord, Show)

instance Exception ConvErr

newtype ConvM a = ConvM {unConvM :: ReaderT Attrs (Except ConvErr) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader Attrs, MonadError ConvErr)

instance MonadFail ConvM where
  fail = throwError . ConvErrFail . T.pack

runConvM :: ConvM a -> Attrs -> Either ConvErr a
runConvM m = runExcept . runReaderT (unConvM m)

mkConvM :: (Attrs -> Either ConvErr a) -> ConvM a
mkConvM f = ask >>= \as -> either throwError pure (f as)

lookupM :: Text -> ConvM (Maybe Datum)
lookupM k = asks (attrsLookup k)

lookupDefaultM :: Datum -> Text -> ConvM Datum
lookupDefaultM v k = asks (attrsLookupDefault v k)

getM :: Text -> ConvM Datum
getM k = lookupM k >>= maybe (throwError (ConvErrMissing k)) pure

matchM :: DatumProxy a -> Datum -> ConvM a
matchM = error "TODO"

exclusiveM :: (Foldable f) => f Text -> ConvM (Text, Datum)
exclusiveM = exclusiveM' . Set.fromList . toList

exclusiveM' :: Set Text -> ConvM (Text, Datum)
exclusiveM' ks0 = go Nothing (toList ks0)
 where
  go !racc = \case
    [] -> case racc of
      Nothing -> throwError (ConvErrMissingAll ks0)
      Just kv -> pure kv
    k : ks -> do
      mv <- lookupM k
      case mv of
        Nothing -> go racc ks
        Just v -> case racc of
          Nothing -> go (Just (k, v)) ks
          Just (k', _) -> throwError (ConvErrExclusive ks0 k' k)

branchM :: (Foldable f) => f (Text, Datum -> ConvM a) -> ConvM a
branchM = branchM' . Map.fromList . toList

branchM' :: Map Text (Datum -> ConvM a) -> ConvM a
branchM' kfs = do
  (k, v) <- exclusiveM' (Map.keysSet kfs)
  let f = kfs Map.! k
  f v
