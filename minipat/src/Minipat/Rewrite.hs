-- | Utilities for rewriting patterns
module Minipat.Rewrite
  ( RwErr (..)
  , RwT
  , Rw
  , askRw
  , asksRw
  , throwRw
  , wrapRw
  , PatRw
  , rewrite
  , PatRwM
  , rewriteM
  , PatOvh
  , overhaul
  , PatOvhM
  , overhaulM
  )
where

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
  -- ^ Path of all locations from the root (closest at end)
  , rwErrReason :: !e
  }
  deriving stock (Eq, Ord, Show)

instance
  (Show b, Typeable b, Show e, Typeable e)
  => Exception (RwErr b e)

-- | As we rewrite, we keep track of all locations from the root (snocing)
type RwT b = ReaderT (NESeq b)

type Rw b = Reader (NESeq b)

askRw :: (Monad m) => RwT b m b
askRw = asks NESeq.last

asksRw :: (Monad m) => (b -> c) -> RwT b m c
asksRw f = asks (f . NESeq.last)

throwRw :: (MonadError (RwErr e b) m) => e -> RwT b m a
throwRw e = ask >>= \bs -> throwError (RwErr bs e)

wrapRw :: (Monad m) => A.PatX b a (A.UnPat b a) -> RwT b m (A.UnPat b a)
wrapRw = asksRw . flip JotP

type PatRw b a x = A.PatX b a x -> Rw b x

rewrite :: PatRw b a x -> A.UnPat b a -> x
rewrite f = runIdentity . rewriteM (f . fmap runIdentity)

type PatRwM b a m x = A.PatX b a (m x) -> RwT b m x

-- | Rewrite just the current pattern constructors (ignoring embedded patterns)
rewriteM :: PatRwM b a m x -> A.UnPat b a -> m x
rewriteM f (JotP b0 pf0) = go (NESeq.singleton b0) pf0
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
      A.PatExtent t -> A.PatExtent (fmap g t)
      A.PatGroup gs -> A.PatGroup (fmap g gs)
      A.PatMod m -> A.PatMod (bimap f g m)
      A.PatPoly p -> A.PatPoly (fmap g p)

instance Bifoldable (TapF a) where
  bifoldr f g z0 (TapF p0) = go z0 p0
   where
    go z = \case
      A.PatPure _ -> z
      A.PatSilence -> z
      A.PatExtent t -> foldr g z t
      A.PatGroup gs -> foldr g z gs
      A.PatMod m -> bifoldr f g z m
      A.PatPoly p -> foldr g z p

instance Bitraversable (TapF a) where
  bitraverse f g = fmap TapF . go . unTapF
   where
    go = \case
      A.PatPure a -> pure (A.PatPure a)
      A.PatSilence -> pure A.PatSilence
      A.PatExtent t -> fmap A.PatExtent (traverse g t)
      A.PatGroup gs -> fmap A.PatGroup (traverse g gs)
      A.PatMod m -> fmap A.PatMod (bitraverse f g m)
      A.PatPoly p -> fmap A.PatPoly (traverse g p)

type PatOvh b = forall a. PatRw b a (A.UnPat b a)

overhaul :: PatOvh b -> A.UnPat b a -> A.UnPat b a
overhaul f = runIdentity . overhaulM (f . fmap runIdentity)

type PatOvhM b m = forall a. PatRwM b a m (A.UnPat b a)

-- Rewrite all pattern constructors *polymorphically* (includes embedded patterns)
overhaulM :: (Monad m) => PatOvhM b m -> A.UnPat b a -> m (A.UnPat b a)
overhaulM f (JotP b0 pf0) = goOvhM f (NESeq.singleton b0) pf0

goOvhM :: (Monad m) => PatOvhM b m -> NESeq b -> A.PatX b a (A.UnPat b a) -> m (A.UnPat b a)
goOvhM f bs pf = do
  pf' <- fmap unTapF (bitraverse (fmap A.Pat . pushOvhM f bs . A.unPat) (pure . pushOvhM f bs) (TapF pf))
  runReaderT (f pf') bs

pushOvhM :: (Monad m) => PatOvhM b m -> NESeq b -> A.UnPat b a -> m (A.UnPat b a)
pushOvhM f bs (JotP b pf) = goOvhM f (bs NESeq.|> b) pf
