module Minipat.Midi.Count where

import Control.Exception (Exception)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Writer (MonadWriter (..), WriterT (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Typeable (Typeable)

newtype ErrCounts e = ErrCounts {unErrCounts :: Map e Int}
  deriving stock (Eq, Ord, Show)

instance (Show e, Typeable e) => Exception (ErrCounts e)

instance (Ord e) => Semigroup (ErrCounts e) where
  ErrCounts m1 <> ErrCounts m2 = ErrCounts (Map.unionWith (+) m1 m2)

instance (Ord e) => Monoid (ErrCounts e) where
  mempty = ErrCounts Map.empty

countErr :: e -> ErrCounts e
countErr e = ErrCounts (Map.singleton e 1)

hasErrs :: ErrCounts e -> Bool
hasErrs = not . Map.null . unErrCounts

rethrowCounts :: (Show e, Typeable e) => (MonadThrow m) => ErrCounts e -> m ()
rethrowCounts c = when (hasErrs c) (throwM c)

newtype CountM e r a = CountM {unCountM :: ReaderT r (WriterT (ErrCounts e) IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r, MonadWriter (ErrCounts e))

throwErrM :: (Ord e) => e -> CountM e s ()
throwErrM = tell . countErr

runCountM :: CountM e r a -> r -> IO (a, ErrCounts e)
runCountM (CountM m) r = runWriterT (runReaderT m r)
