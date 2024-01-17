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
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Identity (runIdentity)
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

type RwM e b = ReaderT (NESeq b) (Except (RwErr e b))

askRw :: RwM e b b
askRw = asks NESeq.last

asksRw :: (b -> c) -> RwM e b c
asksRw f = asks (f . NESeq.last)

throwRw :: e -> RwM e b a
throwRw e = ask >>= \bs -> throwError (RwErr bs e)

finishRw :: Except (RwErr e b) a -> Either (RwErr e b) a
finishRw = runExcept

wrapRw :: A.PatX b a (A.UnPat b a) -> RwM e b (A.UnPat b a)
wrapRw = asksRw . flip JotP

type PatRw b c a z = A.PatX b a (A.UnPat c z) -> Reader (NESeq b) (A.UnPat c z)

rewrite :: PatRw b c a z -> A.Pat b a -> A.Pat c z
rewrite f = runIdentity . rewriteM f

type PatRwM b c m a z = A.PatX b a (A.UnPat c z) -> ReaderT (NESeq b) m (A.UnPat c z)

rewriteM :: (Monad m) => PatRwM b c m a z -> A.Pat b a -> m (A.Pat c z)
rewriteM f (A.Pat (JotP b0 pf0)) = fmap A.Pat (runReaderT (go pf0) (NESeq.singleton b0))
 where
  go pf = traverse push pf >>= f
  push (JotP b pf) = local (NESeq.|> b) (go pf)

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

type PatOvM b m = forall x. PatRwM b b m x x

overhaulM :: (Monad m) => PatOvM b m -> A.Pat b a -> m (A.Pat b a)
overhaulM f (A.Pat (JotP b0 pf0)) = fmap A.Pat (runReaderT (goOvM f pf0) (NESeq.singleton b0))

goOvM :: (Monad m) => PatOvM b m -> A.PatX b a (A.UnPat b a) -> ReaderT (NESeq b) m (A.UnPat b a)
goOvM f pf = do
  pf' <- fmap unTapF (bitraverse (fmap A.Pat . pushOvM f . A.unPat) (pushOvM f) (TapF pf))
  f pf'

pushOvM :: (Monad m) => PatOvM b m -> A.UnPat b a -> ReaderT (NESeq b) m (A.UnPat b a)
pushOvM f (JotP b pf) = local (NESeq.|> b) (goOvM f pf)
