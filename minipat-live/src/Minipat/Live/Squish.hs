module Minipat.Live.Squish where

class Squish q a where
  squish :: a -> q

squishMerge :: (Semigroup q, Squish q a, Squish q b) => a -> b -> q
squishMerge a b = squish a <> squish b

instance {-# OVERLAPPABLE #-} Squish q q where
  squish = id

instance {-# INCOHERENT #-} (Monoid q, Squish q a) => Squish q (Maybe a) where
  squish = maybe mempty squish
