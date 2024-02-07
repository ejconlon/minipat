{-# LANGUAGE UndecidableInstances #-}

-- | Utilities for rewriting patterns
module Minipat.Rewrite where

import Bowtie (Jot, pattern JotP)
import Control.Exception (Exception)
import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)
import Minipat.Ast (Mod (..), ModType (..), Pat (..), PatF (..), Poly (..), Speed (..), UnPat)

-- * General rewriting (can go in Bowtie)

data AnnoErr k e = AnnoErr
  { annoErrKey :: !k
  , annoErrVal :: !e
  }
  deriving stock (Eq, Ord, Show)

instance
  (Show k, Typeable k, Show e, Typeable e)
  => Exception (AnnoErr k e)

unwrapAnnoErr :: Either (AnnoErr k Void) a -> a
unwrapAnnoErr = either (absurd . annoErrVal) id

newtype RwT k e m a = RwT {unRwT :: ReaderT (NESeq k) (ExceptT (AnnoErr k e) m) a}
  deriving newtype (Functor, Applicative, Monad)

type Rw k e = RwT k e Identity

instance MonadTrans (RwT k e) where
  lift = RwT . lift . lift

runRwT :: RwT k e m a -> k -> m (Either (AnnoErr k e) a)
runRwT m = runExceptT . runReaderT (unRwT m) . NESeq.singleton

runRw :: Rw k e a -> k -> Either (AnnoErr k e) a
runRw m = runIdentity . runRwT m

pushRw :: (Monad m) => k -> RwT k e m a -> RwT k e m a
pushRw b m = RwT (local (NESeq.|> b) (unRwT m))

peekRw :: (Monad m) => RwT k e m k
peekRw = RwT (asks NESeq.last)

peeksRw :: (Monad m) => (k -> a) -> RwT k e m a
peeksRw f = RwT (asks (f . NESeq.last))

askRw :: (Monad m) => RwT k e m (NESeq k)
askRw = RwT ask

asksRw :: (Monad m) => (NESeq k -> a) -> RwT k e m a
asksRw f = RwT (asks f)

throwRw :: (Monad m) => e -> RwT k e m a
throwRw e = RwT (asks NESeq.last >>= \b -> throwError (AnnoErr b e))

instance (MonadReader r m) => MonadReader r (RwT k e m) where
  ask = lift ask
  reader f = lift (reader f)
  local f m = RwT $ do
    bs <- ask
    ea <- lift (lift (local f (runExceptT (runReaderT (unRwT m) bs))))
    either throwError pure ea

instance (MonadState s m) => MonadState s (RwT k e m) where
  get = lift get
  put = lift . put
  state f = lift (state f)

instance (MonadIO m) => MonadIO (RwT k e m) where
  liftIO = lift . liftIO

wrapRw :: g a (Jot g k a) -> Rw k e (Jot g k a)
wrapRw = peeksRw . flip JotP

jotCataRw :: (Bitraversable g) => (g a z -> Rw k e z) -> Jot g k a -> Rw k e z
jotCataRw f = jotCataRwT (bitraverse pure id >=> f)

jotCataRwT :: (Monad m, Bitraversable g) => (g a (RwT k e m z) -> RwT k e m z) -> Jot g k a -> RwT k e m z
jotCataRwT f = goJ
 where
  goJ (JotP b g) = pushRw b (goG g)
  goG g = f (fmap goJ g)

-- * Pattern rewriting

patCataRw :: (PatF b a z -> Rw b e z) -> Pat b a -> Rw b e z
patCataRw f = jotCataRw f . unPat

patCataRwT :: (Monad m) => (PatF b a (RwT b e m z) -> RwT b e m z) -> Pat b a -> RwT b e m z
patCataRwT f = jotCataRwT f . unPat

patNatRw :: (forall x. PatF b x (UnPat b x) -> Rw b e (UnPat b x)) -> Pat b a -> Rw b e (Pat b a)
patNatRw f = patNatRwT (bitraverse pure id >=> f)

patNatRwT
  :: (Monad m) => (forall x. PatF b x (RwT b e m (UnPat b x)) -> RwT b e m (UnPat b x)) -> Pat b a -> RwT b e m (Pat b a)
patNatRwT f = goP
 where
  goP = fmap Pat . goJ . unPat
  goJ (JotP b pf) = pushRw b (goG pf >>= f)
  goG = \case
    PatPure a -> pure (PatPure a)
    PatSilence -> pure PatSilence
    PatShort s -> pure (PatShort s)
    PatGroup gs -> pure (PatGroup (fmap goJ gs))
    PatMod (Mod r m) -> fmap (PatMod . Mod (goJ r)) (goM m)
    PatPoly (Poly rs mi) -> pure (PatPoly (Poly (fmap goJ rs) mi))
  goM = \case
    ModTypeDegrade d -> pure (ModTypeDegrade d)
    ModTypeEuclid e -> pure (ModTypeEuclid e)
    ModTypeSpeed s -> fmap ModTypeSpeed (goS s)
    ModTypeElongate e -> pure (ModTypeElongate e)
    ModTypeReplicate r -> pure (ModTypeReplicate r)
  goS (Speed d p) = fmap (Speed d) (patNatRwT f p)

runPatRw :: (Pat b a -> Rw b e z) -> Pat b a -> Either (AnnoErr b e) z
runPatRw g p@(Pat (JotP b _)) = runRw (g p) b

runPatRwT :: (Pat b a -> RwT b e m z) -> Pat b a -> m (Either (AnnoErr b e) z)
runPatRwT g p@(Pat (JotP b _)) = runRwT (g p) b

patMapInfo :: (b -> c) -> Pat b a -> Pat c a
patMapInfo f = goP
 where
  goP = Pat . goJ . unPat
  goJ (JotP b pf) = JotP (f b) (goG (fmap goJ pf))
  goG = \case
    PatPure a -> PatPure a
    PatSilence -> PatSilence
    PatShort s -> PatShort s
    PatGroup gs -> PatGroup gs
    PatMod (Mod r m) -> PatMod (Mod r (goM m))
    PatPoly (Poly rs mi) -> PatPoly (Poly rs mi)
  goM = \case
    ModTypeDegrade d -> ModTypeDegrade d
    ModTypeEuclid e -> ModTypeEuclid e
    ModTypeSpeed s -> ModTypeSpeed (goS s)
    ModTypeElongate e -> ModTypeElongate e
    ModTypeReplicate r -> ModTypeReplicate r
  goS (Speed d p) = Speed d (patMapInfo f p)
