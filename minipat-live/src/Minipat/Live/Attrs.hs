{-# LANGUAGE OverloadedStrings #-}

-- | This subproject is all about assembling streams of 'Attrs' to send to backends.
-- `Map String Any` is a terrible type in general, but it is the least bad
-- option for assembling events incrementally! See 'Minipat.Live.Convert' for
-- a structured way to map this to a more appropriate type for a given backend.
module Minipat.Live.Attrs
  ( Attrs
  , attrsSingleton
  , attrsFromList
  , attrsLookup
  , attrsLookupDefault
  , attrsInsert
  , attrsInsertDefault
  , attrsDelete
  , attrsToList
  , DupeAttrErr
  , attrsTryInsert
  , MissingAttrErr
  , attrsTryLookup
  , attrsUnalias
  , Attr (..)
  , IsAttrs (..)
  , attrsMerge
  )
where

import Control.Exception (Exception)
import Control.Monad (foldM)
import Dahdit.Midi.Osc (Datum (..), IsDatum (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Minipat.Live.Datum (prettyDatum)
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as P

newtype Attrs = Attrs {unAttrs :: Map Text Datum}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Semigroup Attrs where
  -- Flip order to allow keys on the right to win
  Attrs m1 <> Attrs m2 = Attrs (m2 <> m1)

instance Monoid Attrs where
  mempty = Attrs Map.empty

instance Pretty Attrs where
  pretty as = P.hsep ("{" : P.punctuate "," (fmap (\(k, v) -> P.hcat [pretty k, ": ", prettyDatum v]) (attrsToList as)) ++ ["}"])

attrsSingleton :: Text -> Datum -> Attrs
attrsSingleton k v = Attrs (Map.singleton k v)

attrsFromList :: [(Text, Datum)] -> Attrs
attrsFromList = Attrs . Map.fromList

attrsLookup :: Text -> Attrs -> Maybe Datum
attrsLookup k (Attrs m) = Map.lookup k m

attrsLookupDefault :: Datum -> Text -> Attrs -> Datum
attrsLookupDefault v k (Attrs m) = Map.findWithDefault v k m

attrsInsert :: Text -> Datum -> Attrs -> Attrs
attrsInsert k v (Attrs m) = Attrs (Map.insert k v m)

attrsInsertDefault :: Text -> Datum -> Attrs -> Attrs
attrsInsertDefault k v a@(Attrs m) = case Map.lookup k m of
  Nothing -> Attrs (Map.insert k v m)
  Just _ -> a

attrsDelete :: Text -> Attrs -> Attrs
attrsDelete k (Attrs m) = Attrs (Map.delete k m)

attrsToList :: Attrs -> [(Text, Datum)]
attrsToList = Map.toList . unAttrs

newtype DupeAttrErr = DupeAttrErr {unDupeAttrErr :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Exception DupeAttrErr

attrsTryInsert :: Text -> Datum -> Attrs -> Either DupeAttrErr Attrs
attrsTryInsert k v m =
  case attrsLookup k m of
    Nothing -> Right (attrsInsert k v m)
    Just _ -> Left (DupeAttrErr k)

newtype MissingAttrErr = MissingAttrErr {unMissingAttrErr :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

attrsTryLookup :: Text -> Attrs -> Either MissingAttrErr Datum
attrsTryLookup k = maybe (Left (MissingAttrErr k)) Right . attrsLookup k

attrsUnalias :: [(Text, Text)] -> Attrs -> Either DupeAttrErr Attrs
attrsUnalias as m0 = foldM go m0 as
 where
  go !m (x, y) = do
    case attrsLookup x m of
      Nothing -> pure m
      Just v -> attrsTryInsert y v (attrsDelete x m)

-- | For when you need something anonymous with an 'IsAttrs' instance.
data Attr a = Attr
  { attrKey :: !Text
  , attrVal :: !a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

class IsAttrs a where
  toAttrs :: a -> Attrs

instance (IsDatum a) => IsAttrs (Attr a) where
  toAttrs (Attr k v) = attrsSingleton k (toDatum v)

instance IsAttrs Attrs where
  toAttrs = id

instance (IsAttrs a) => IsAttrs (Maybe a) where
  toAttrs = maybe mempty toAttrs

attrsMerge :: (IsAttrs a, IsAttrs b) => a -> b -> Attrs
attrsMerge a b = toAttrs a <> toAttrs b
