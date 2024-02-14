{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}

module Minipat.Dirt.Strip where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception, SomeException (..))
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Minipat.Eval (evalPat)
import Minipat.Parser (P)
import Minipat.Stream (Stream)
import Minipat.Stream qualified as S

-- Attempting to add a few things to Streams
-- 1) Tracking errors for later logging
-- 2) IsString instance for seamless parsing
newtype Strip (k :: k1) (a :: Type) = Strip {unStrip :: Either SomeException (Stream a)}
  deriving stock (Functor)

type role Strip phantom nominal

instance Applicative (Strip k) where
  pure = Strip . Right . pure
  liftA2 f (Strip ca) (Strip cb) = Strip (liftA2 (liftA2 f) ca cb)

instance Semigroup (Strip k a) where
  Strip es1 <> Strip es2 = Strip (liftA2 (<>) es1 es2)
  sconcat = Strip . fmap sconcat . traverse unStrip

instance Monoid (Strip k a) where
  mempty = Strip (Right mempty)
  mconcat = Strip . fmap mconcat . traverse unStrip

instance Alternative (Strip k) where
  empty = mempty
  (<|>) = (<>)

stripCast :: Strip k a -> Strip j a
stripCast = coerce

stripMap :: (Stream a -> Stream b) -> Strip k a -> Strip j b
stripMap f (Strip c) = Strip (fmap f c)

stripBind :: Strip k a -> (Stream a -> Strip j b) -> Strip j b
stripBind (Strip c) f = Strip (c >>= unStrip . f)

stripThrow :: (Exception e) => e -> Strip k a
stripThrow = Strip . Left . SomeException

stripFilter :: (a -> Bool) -> Strip k a -> Strip j a
stripFilter = stripMap . S.streamFilter

class StripParse k a | k -> a where
  stripParse :: Proxy k -> Text -> Strip k a

instance (StripParse k a) => IsString (Strip k a) where
  fromString = stripParse (Proxy :: Proxy k) . fromString

stripEval :: P a -> Text -> Strip k a
stripEval ee txt = Strip (evalPat ee txt)

-- instance Pattern (Strip k) where
