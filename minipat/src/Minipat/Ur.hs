module Minipat.Ur where

import Bowtie.Rewrite (AnnoErr)
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Data.Map.Strict qualified as Map
import Minipat.Ast (Ident, Pat, Pattern (..), Select (..))
import Minipat.Interp (InterpErr, customInterpPat)
import Minipat.Time (CycleDelta (..))

data UrErr k =
    UrErrPat !k
  | UrErrXform !Ident
  deriving stock (Eq, Ord, Show)

instance (Show k, Typeable k) => Exception (UrErr k)

ur
  :: (Pattern f, Ord k)
  => CycleDelta
  -> Pat b (Select Ident k)
  -> [(k, f a)]
  -> [(Ident, f a -> f a)]
  -> Either (AnnoErr b (InterpErr (UrErr k))) (f a)
ur del pat (Map.fromList -> xs) (Map.fromList -> ys) =
  ur' del pat (`Map.lookup` xs) (`Map.lookup` ys)

urUse :: (k -> Maybe (f a)) -> (Ident -> Maybe (f a -> f a)) -> Select Ident k -> Either (UrErr k) (f a)
urUse findPat findXform (Select k mx) =
  case findPat k of
    Nothing -> Left (UrErrPat k)
    Just p -> case mx of
      Nothing -> Right p
      Just x -> case findXform x of
        Nothing -> Left (UrErrXform x)
        Just f -> Right (f p)

ur'
  :: (Pattern f)
  => CycleDelta
  -> Pat b (Select Ident k)
  -> (k -> Maybe (f a))
  -> (Ident -> Maybe (f a -> f a))
  -> Either (AnnoErr b (InterpErr (UrErr k))) (f a)
ur' del pat findPat findXform = fmap (patSlowBy (unCycleDelta del)) (customInterpPat (urUse findPat findXform) pat)
