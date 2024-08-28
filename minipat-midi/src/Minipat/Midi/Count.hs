module Minipat.Midi.Count where

import Control.Monad (when)
import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Control.Monad.Reader (ReaderT (..), MonadReader)
import Control.Monad.Writer (WriterT (..), MonadWriter (..))
import Data.Typeable (Typeable)
import Control.Monad.Catch (MonadThrow (..))

newtype ErrCounts e = ErrCounts { unErrCounts :: Map e Int }
  deriving stock (Eq, Ord, Show)

instance (Show e, Typeable e) => Exception (ErrCounts e)

instance Ord e => Semigroup (ErrCounts e) where
  ErrCounts m1 <> ErrCounts m2 = ErrCounts (Map.unionWith (+) m1 m2)

instance Ord e => Monoid (ErrCounts e) where
  mempty = ErrCounts Map.empty

countErr :: e -> ErrCounts e
countErr e = ErrCounts (Map.singleton e 1)

hasErrs :: ErrCounts e -> Bool
hasErrs = not . Map.null . unErrCounts

rethrowCounts :: (Show e, Typeable e) => MonadThrow m => ErrCounts e -> m ()
rethrowCounts c = when (hasErrs c) (throwM c)

newtype CountM e r a = CountM { unCountM :: ReaderT r (WriterT (ErrCounts e) IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r, MonadWriter (ErrCounts e))

countErrM :: Ord e => e -> CountM e s ()
countErrM = tell . countErr

runCountM :: CountM e r a -> r -> IO (a, ErrCounts e)
runCountM (CountM m) = runWriterT . runReaderT m

