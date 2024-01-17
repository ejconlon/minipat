module Minipat.Rewrite where

-- ( RwErr (..)
-- , RwM
-- , finishRw
-- , throwRw
-- , askRw
-- , asksRw
-- , rewrite
-- , rewriteM
-- )

import Bowtie (pattern JotP)
import Control.Exception (Exception)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader (..), Reader, ReaderT (..), asks)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Typeable (Typeable)
import Minipat.Ast qualified as A

-- | Path components are pushed onto the end as
-- we descend the tree
data RwErr e b = RwErr
  { rwErrPath :: !(NESeq b)
  , rwErrReason :: !e
  }
  deriving stock (Eq, Ord, Show)

instance
  (Show b, Typeable b, Show e, Typeable e)
  => Exception (RwErr b e)

type Rw b = Reader (NESeq b)

type RwT b = ReaderT (NESeq b)

askRw :: (Monad m) => RwT b m b
askRw = asks NESeq.last

asksRw :: (Monad m) => (b -> c) -> RwT b m c
asksRw f = asks (f . NESeq.last)

throwRw :: (MonadError (RwErr e b) m) => e -> RwT b m a
throwRw e = ask >>= \bs -> throwError (RwErr bs e)

wrapRw :: (Monad m) => A.PatX b a (A.UnPat b a) -> RwT b m (A.UnPat b a)
wrapRw = asksRw . flip JotP

type PatRw b c a z = A.PatX b a (A.UnPat c z) -> Rw b (A.UnPat c z)

rewrite :: PatRw b c a z -> A.Pat b a -> A.Pat c z
rewrite f = runIdentity . rewriteM (f . fmap runIdentity)

type PatRwM b c m a z = A.PatX b a (m (A.UnPat c z)) -> RwT b m (A.UnPat c z)

rewriteM :: (Monad m) => PatRwM b c m a z -> A.Pat b a -> m (A.Pat c z)
rewriteM f (A.Pat (JotP b0 pf0)) = fmap A.Pat (go (NESeq.singleton b0) pf0)
 where
  go bs pf = runReaderT (f (fmap (push bs) pf)) bs
  push bs (JotP b pf) = go (bs NESeq.|> b) pf

-- Targets both positions filled by patterns
newtype TapF a s r = TapF {unTapF :: A.PatF s a r}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Functor, Foldable)

instance Bifunctor (TapF a) where
  bimap f g = TapF . go . unTapF
   where
    go = \case
      A.PatPure a -> A.PatPure a
      A.PatSilence -> A.PatSilence
      A.PatTime t -> A.PatTime (fmap g t)
      A.PatGroup gs -> A.PatGroup (fmap g gs)
      A.PatMod m -> A.PatMod (bimap f g m)
      A.PatPoly p -> A.PatPoly (fmap g p)

instance Bifoldable (TapF a) where
  bifoldr f g z0 (TapF p0) = go z0 p0
   where
    go z = \case
      A.PatPure _ -> z
      A.PatSilence -> z
      A.PatTime t -> foldr g z t
      A.PatGroup gs -> foldr g z gs
      A.PatMod m -> bifoldr f g z m
      A.PatPoly p -> foldr g z p

instance Bitraversable (TapF a) where
  bitraverse f g = fmap TapF . go . unTapF
   where
    go = \case
      A.PatPure a -> pure (A.PatPure a)
      A.PatSilence -> pure A.PatSilence
      A.PatTime t -> fmap A.PatTime (traverse g t)
      A.PatGroup gs -> fmap A.PatGroup (traverse g gs)
      A.PatMod m -> fmap A.PatMod (bitraverse f g m)
      A.PatPoly p -> fmap A.PatPoly (traverse g p)

type PatOv b = forall x. PatRw b b x x

overhaul :: PatOv b -> A.Pat b a -> A.Pat b a
overhaul f = runIdentity . overhaulM (f . fmap runIdentity)

type PatOvM b m = forall x. PatRwM b b m x x

overhaulM :: (Monad m) => PatOvM b m -> A.Pat b a -> m (A.Pat b a)
overhaulM f (A.Pat (JotP b0 pf0)) = fmap A.Pat (goOvM f (NESeq.singleton b0) pf0)

goOvM :: (Monad m) => PatOvM b m -> NESeq b -> A.PatX b a (A.UnPat b a) -> m (A.UnPat b a)
goOvM f bs pf = do
  pf' <- fmap unTapF (bitraverse (fmap A.Pat . pushOvM f bs . A.unPat) (pure . pushOvM f bs) (TapF pf))
  runReaderT (f pf') bs

pushOvM :: (Monad m) => PatOvM b m -> NESeq b -> A.UnPat b a -> m (A.UnPat b a)
pushOvM f bs (JotP b pf) = goOvM f (bs NESeq.|> b) pf
