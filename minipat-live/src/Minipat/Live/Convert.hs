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
  )
where

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
import Minipat.Live.Attrs (Attrs, attrsLookup)
import Minipat.Live.Datum (DatumProxy, DatumTypeErr (..), castDatum)

data ConvErr
  = ConvErrFail !Text
  | ConvErrMissing !Text
  | ConvErrMissingAll !(Set Text)
  | ConvErrExclusive !(Set Text) !Text !Text
  | ConvErrMatch !Text !DatumType !DatumType
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

lookupM :: Text -> DatumProxy a -> ConvM (Maybe a)
lookupM k p = asks (attrsLookup k) >>= traverse (matchM k p)

defaultM :: Text -> DatumProxy a -> a -> ConvM a
defaultM k p d = asks (attrsLookup k) >>= maybe (pure d) (matchM k p)

getM :: Text -> DatumProxy a -> ConvM a
getM k p = lookupM k p >>= maybe (throwError (ConvErrMissing k)) pure

matchM :: Text -> DatumProxy a -> Datum -> ConvM a
matchM k p v = case castDatum p v of
  Left (DatumTypeErr expected actual) -> throwError (ConvErrMatch k expected actual)
  Right val -> pure val

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
      mv <- asks (attrsLookup k)
      case mv of
        Nothing -> go racc ks
        Just v -> case racc of
          Nothing -> go (Just (k, v)) ks
          Just (k', _) -> throwError (ConvErrExclusive ks0 k' k)

data Branch a where
  Branch :: DatumProxy z -> (z -> ConvM a) -> Branch a

runBranch :: Branch a -> Text -> Datum -> ConvM a
runBranch (Branch p f) k v = matchM k p v >>= f

branchM :: (Foldable f) => f (Text, Branch a) -> ConvM a
branchM = branchM' . Map.fromList . toList

branchM' :: Map Text (Branch a) -> ConvM a
branchM' kfs = do
  (k, v) <- exclusiveM' (Map.keysSet kfs)
  let b = kfs Map.! k
  runBranch b k v
