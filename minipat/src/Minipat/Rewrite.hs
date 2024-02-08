{-# LANGUAGE UndecidableInstances #-}

-- | Utilities for rewriting patterns
module Minipat.Rewrite where

import Bowtie (pattern JotP)
import Bowtie.Rewrite (AnnoErr, Rw, RwT, embedRwT, jotRw, jotRwT, pushRw, runRwT)
import Control.Monad ((>=>))
import Control.Monad.Identity (Identity (..))
import Data.Bitraversable (Bitraversable (..))
import Minipat.Ast (Mod (..), ModType (..), Pat (..), PatF (..), Poly (..), Speed (..), UnPat)

patRw :: (PatF b a z -> Rw b e z) -> Pat b a -> Either (AnnoErr b e) z
patRw f = jotRw f . unPat

patRwT :: (Monad m) => (PatF b a (RwT b e m z) -> RwT b e m z) -> Pat b a -> m (Either (AnnoErr b e) z)
patRwT f = jotRwT f . unPat

patNatRw :: (forall x. PatF b x (UnPat b x) -> Rw b e (UnPat b x)) -> Pat b a -> Either (AnnoErr b e) (Pat b a)
patNatRw f = runIdentity . patNatRwT (bitraverse pure id >=> f)

-- TODO can use jotRwT (goG pf >>= f) or something?
patNatRwT
  :: (Monad m)
  => (forall x. PatF b x (RwT b e m (UnPat b x)) -> RwT b e m (UnPat b x))
  -> Pat b a
  -> m (Either (AnnoErr b e) (Pat b a))
patNatRwT f p0@(Pat (JotP b0 _)) = runRwT (goP p0) b0
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
  goS (Speed d p) = fmap (Speed d) (embedRwT (patNatRwT f p))

-- TODO just make Bifunctor instance
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
