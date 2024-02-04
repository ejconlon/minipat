{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}

module Minipat.Dirt.Flow where

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
newtype Flow (k :: k1) (a :: Type) = Flow {unFlow :: Either SomeException (Stream a)}
  deriving stock (Functor)

type role Flow phantom nominal

instance Applicative (Flow k) where
  pure = Flow . Right . pure
  liftA2 f (Flow ca) (Flow cb) = Flow (liftA2 (liftA2 f) ca cb)

instance Semigroup (Flow k a) where
  Flow es1 <> Flow es2 = Flow (liftA2 (<>) es1 es2)
  sconcat = Flow . fmap sconcat . traverse unFlow

instance Monoid (Flow k a) where
  mempty = Flow (Right mempty)
  mconcat = Flow . fmap mconcat . traverse unFlow

flowMap :: (Stream a -> Stream b) -> Flow k a -> Flow j b
flowMap f (Flow c) = Flow (fmap f c)

flowBind :: Flow k a -> (Stream a -> Flow j b) -> Flow j b
flowBind (Flow c) f = Flow (c >>= unFlow . f)

flowThrow :: (Exception e) => e -> Flow k a
flowThrow = Flow . Left . SomeException

flowFilter :: (a -> Bool) -> Flow k a -> Flow j a
flowFilter = flowMap . S.streamFilter

class (Show (FlowEvalErr k), Typeable (FlowEvalErr k)) => FlowEval k a | k -> a where
  type FlowEvalErr k :: Type
  flowEvalEnv :: Proxy k -> EvalEnv (FlowEvalErr k) a

instance (FlowEval k a) => IsString (Flow k a) where
  fromString = flowEval (flowEvalEnv (Proxy :: Proxy k)) . fromString

flowEval :: (Show e, Typeable e) => EvalEnv e a -> Text -> Flow k a
flowEval ee txt = Flow (evalPat ee txt)
