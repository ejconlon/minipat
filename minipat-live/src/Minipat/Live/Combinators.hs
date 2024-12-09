{-# LANGUAGE OverloadedLists #-}

-- | Useful combinators and shorthands
module Minipat.Live.Combinators
  ( S
  , setIn
  , (#)
  , fast
  , slow
  , fastBy
  , slowBy
  , lateBy
  , earlyBy
  , pieces
  , fastCat
  , slowCat
  , fastList
  , slowList
  , fastAppend
  , slowAppend
  , alt
  , rand
  )
where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Minipat.Classes (Flow (..))
import Minipat.EStream (EStream)
import Minipat.EStream qualified as E
import Minipat.Live.Attrs (Attrs, ToAttrs, attrsMerge)
import Minipat.Time (CycleDelta, CycleTime)

type S = EStream

setIn, (#) :: (ToAttrs a, ToAttrs b) => S a -> S b -> S Attrs
setIn = flowInnerApply attrsMerge
(#) = setIn

fast, slow :: S Rational -> S a -> S a
fast = E.estreamFast
slow = E.estreamSlow

fastBy, slowBy :: Rational -> S a -> S a
fastBy = E.estreamFastBy
slowBy = E.estreamSlowBy

lateBy, earlyBy :: CycleDelta -> S a -> S a
lateBy = E.estreamLateBy
earlyBy = E.estreamEarlyBy

pieces :: Seq (CycleTime, CycleTime, S a) -> S a
pieces = E.estreamPar . fmap (\(start, end, stream) -> E.estreamPieces mempty [(start, stream), (end, mempty)])

fastCat :: Seq (S a) -> S a
fastCat = E.estreamSeq

slowCat :: Seq (S a) -> S a
slowCat ss = slowBy (fromIntegral (Seq.length ss)) (fastCat ss)

fastList :: Seq a -> S a
fastList = fastCat . fmap pure

slowList :: Seq a -> S a
slowList as = slowBy (fromIntegral (Seq.length as)) (fastList as)

fastAppend :: S a -> S a -> S a
fastAppend s1 s2 = fastCat [s1, s2]

slowAppend :: S a -> S a -> S a
slowAppend s1 s2 = slowBy 2 (fastAppend s1 s2)

alt :: Seq (S a) -> S a
alt = E.estreamAlt

rand :: Seq (S a) -> S a
rand = E.estreamRand

-- TODO implement combinators like these
-- seqPLoop :: Seq (CycleTime, CycleTime, S a) -> S a
-- rev :: S a -> S a
-- swingBy :: Rational -> S a -> S a
-- swing :: S Rational -> S a -> S a
-- echo
-- off
-- timeCat
-- randCat
-- wrandCat
-- wedge
