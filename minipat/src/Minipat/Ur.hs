module Minipat.Ur
  ( ur
  , urTyped
  )
where

import Bowtie.Rewrite (AnnoErr)
import Control.Exception (Exception, SomeException (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Typeable (Typeable)
import Minipat.Ast (Ident, Pat, Select (..))
import Minipat.Classes (Pattern (..), PatternUnwrap (..))
import Minipat.Eval (PatternEval, evalPat)
import Minipat.Interp (InterpErr, customInterpPat)
import Minipat.Parser (Loc, identP, selectP)
import Minipat.Time (CycleDelta (..))

-- | Thrown when we can't match pattern or effect identifiers in 'ur'
data UrErr k
  = UrErrPat !k
  | UrErrXform !Ident
  deriving stock (Eq, Ord, Show)

instance (Show k, Typeable k) => Exception (UrErr k)

-- | Interpret a pattern over a given number of cycles, splicing in
-- named patterns and effects.
ur
  :: (PatternEval f)
  => CycleDelta
  -> Text
  -> [(Ident, f a)]
  -> [(Ident, f a -> f a)]
  -> Either SomeException (f a)
ur del txt ks xs = do
  pat <- evalPat (selectP identP identP) txt
  let ea = urTyped @Loc del pat ks xs
  either (Left . SomeException) Right ea

-- | 'ur' but skipping the parse step
urTyped
  :: (PatternUnwrap b f, Ord k)
  => CycleDelta
  -> Pat b (Select Ident k)
  -> [(k, f a)]
  -> [(Ident, f a -> f a)]
  -> Either (AnnoErr b (InterpErr (UrErr k))) (f a)
urTyped del pat (Map.fromList -> ks) (Map.fromList -> xs) =
  urInterp del pat (`Map.lookup` ks) (`Map.lookup` xs)

urUse
  :: (Applicative (PatM f))
  => (k -> Maybe (f a))
  -> (Ident -> Maybe (f a -> f a))
  -> Select Ident k
  -> Either (UrErr k) (PatM f (f a))
urUse findPat findXform (Select k mx) =
  case findPat k of
    Nothing -> Left (UrErrPat k)
    Just p -> case mx of
      Nothing -> Right (pure p)
      Just x -> case findXform x of
        Nothing -> Left (UrErrXform x)
        Just f -> Right (pure (f p))

urInterp
  :: (PatternUnwrap b f)
  => CycleDelta
  -> Pat b (Select Ident k)
  -> (k -> Maybe (f a))
  -> (Ident -> Maybe (f a -> f a))
  -> Either (AnnoErr b (InterpErr (UrErr k))) (f a)
urInterp (CycleDelta del) pat findPat findXform =
  let x = customInterpPat (urUse findPat findXform) pat
  in  if del == 1 then x else fmap (patSlowBy del) x
