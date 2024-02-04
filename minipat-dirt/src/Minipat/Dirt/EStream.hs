{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}

module Minipat.Dirt.EStream where

import Control.Exception (Exception, SomeException (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Minipat.Eval (EvalEnv, evalPat)
import Minipat.Stream (Stream)
import Minipat.Stream qualified as S

-- Attempting to add a few things to Streams
-- 1) Tracking errors for later logging
-- 2) IsString instance for seamless parsing
newtype EStream (k :: k1) (a :: Type) = EStream {unEStream :: Either SomeException (Stream a)}
  deriving stock (Functor)

type role EStream phantom nominal

instance Applicative (EStream k) where
  pure = EStream . Right . pure
  liftA2 f (EStream ca) (EStream cb) = EStream (liftA2 (liftA2 f) ca cb)

instance Semigroup (EStream k a) where
  EStream es1 <> EStream es2 = EStream (liftA2 (<>) es1 es2)
  sconcat = EStream . fmap sconcat . traverse unEStream

instance Monoid (EStream k a) where
  mempty = EStream (Right mempty)
  mconcat = EStream . fmap mconcat . traverse unEStream

estreamMap :: (Stream a -> Stream b) -> EStream k a -> EStream j b
estreamMap f (EStream c) = EStream (fmap f c)

estreamBind :: EStream k a -> (Stream a -> EStream j b) -> EStream j b
estreamBind (EStream c) f = EStream (c >>= unEStream . f)

estreamThrow :: (Exception e) => e -> EStream k a
estreamThrow = EStream . Left . SomeException

estreamFilter :: (a -> Bool) -> EStream k a -> EStream j a
estreamFilter = estreamMap . S.streamFilter

class EStreamEval k e a | k -> e a where
  estreamEvalEnv :: Proxy k -> EvalEnv e a

instance (EStreamEval k e a, Show e, Typeable e) => IsString (EStream k a) where
  fromString = estreamEval @e (estreamEvalEnv (Proxy :: Proxy k)) . fromString

estreamEval :: (Show e, Typeable e) => EvalEnv e a -> Text -> EStream k a
estreamEval ee txt = EStream (evalPat ee txt)
