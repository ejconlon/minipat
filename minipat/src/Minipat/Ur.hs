module Minipat.Ur where

import Data.Map.Strict qualified as Map
import Minipat.Ast (Ident, Pat, Pattern, Select)

-- TODO figure this out
ur
  :: (Pattern f, Ord k)
  => Pat b (Select k Ident)
  -> [(k, f a)]
  -> [(Ident, f a -> f a)]
  -> f a
ur p0 (Map.fromList -> xs) (Map.fromList -> ys) =
  ur' p0 (`Map.lookup` xs) (`Map.lookup` ys)

ur'
  :: (Pattern f, Ord k)
  => Pat b (Select k Ident)
  -> (k -> Maybe (f a))
  -> (Ident -> Maybe (f a -> f a))
  -> f a
ur' = error "TODO"
