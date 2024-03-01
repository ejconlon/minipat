{-# LANGUAGE UndecidableInstances #-}

-- | Instead of simply returning an empty stream when failing to parse a pattern, we can
-- save the error and report it at the top level (after applying all the usual combinators
-- like `#`, `fast`, etc). 'EStream' is a wrapper around 'Stream' that does just that.
module Minipat.EStream
  ( EStream (..)
  , estreamMap
  , estreamLiftA2
  , estreamBind
  , estreamApply
  , estreamThrow
  , estreamFilter
  , estreamFastBy
  , estreamSlowBy
  , estreamFast
  , estreamSlow
  , estreamEarlyBy
  , estreamLateBy
  , estreamEarly
  , estreamLate
  , estreamDegBy
  , estreamDeg
  , estreamSeq
  , estreamRel
  , estreamRep
  , estreamCont
  , estreamEuc
  , estreamRand
  , estreamAlt
  , estreamPar
  , estreamSwitch
  , estreamPieces
  )
where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception, SomeException (..))
import Control.Monad.Identity (Identity (..))
import Data.Kind (Type)
import Data.Semigroup (Semigroup (..))
import Data.Sequence (Seq)
import Minipat.Ast (Euclid)
import Minipat.Classes (Flow (..), Pattern (..), PatternUnwrap (..))
import Minipat.Stream (Stream)
import Minipat.Stream qualified as S
import Minipat.Time (CycleDelta, CycleTime, MergeStrat)

-- | Tracks errors in stream creation for later logging
newtype EStream (a :: Type) = EStream {unEStream :: Either SomeException (Stream a)}
  deriving stock (Functor)

instance Applicative EStream where
  pure = EStream . Right . pure
  liftA2 f (EStream ca) (EStream cb) = EStream (liftA2 (liftA2 f) ca cb)

instance Semigroup (EStream a) where
  EStream es1 <> EStream es2 = EStream (liftA2 (<>) es1 es2)
  sconcat = EStream . fmap sconcat . traverse unEStream

instance Monoid (EStream a) where
  mempty = EStream (Right mempty)
  mconcat = EStream . fmap mconcat . traverse unEStream

instance Alternative EStream where
  empty = mempty
  (<|>) = (<>)

estreamMap :: (Stream a -> Stream b) -> EStream a -> EStream b
estreamMap f (EStream s) = EStream (fmap f s)

estreamLiftA2 :: (Stream a -> Stream b -> Stream c) -> EStream a -> EStream b -> EStream c
estreamLiftA2 f (EStream s1) (EStream s2) = EStream (liftA2 f s1 s2)

estreamBind :: EStream a -> (Stream a -> EStream b) -> EStream b
estreamBind (EStream c) f = EStream (c >>= unEStream . f)

estreamApply :: MergeStrat -> (a -> b -> c) -> EStream a -> EStream b -> EStream c
estreamApply ms = estreamLiftA2 . S.streamApply ms

estreamThrow :: (Exception e) => e -> EStream a
estreamThrow = EStream . Left . SomeException

estreamFilter :: (a -> Bool) -> EStream a -> EStream a
estreamFilter = estreamMap . S.streamFilter

estreamFastBy, estreamSlowBy :: Rational -> EStream a -> EStream a
estreamFastBy = estreamMap . S.streamFastBy
estreamSlowBy = estreamMap . S.streamSlowBy

estreamFast, estreamSlow :: EStream Rational -> EStream a -> EStream a
estreamFast = estreamLiftA2 S.streamFast
estreamSlow = estreamLiftA2 S.streamSlow

estreamEarlyBy, estreamLateBy :: CycleDelta -> EStream a -> EStream a
estreamEarlyBy = estreamMap . S.streamEarlyBy
estreamLateBy = estreamMap . S.streamLateBy

estreamEarly, estreamLate :: EStream CycleDelta -> EStream a -> EStream a
estreamEarly = estreamLiftA2 S.streamEarly
estreamLate = estreamLiftA2 S.streamLate

estreamDegBy :: Rational -> EStream a -> EStream a
estreamDegBy = estreamMap . S.streamDegBy

estreamDeg :: EStream Rational -> EStream a -> EStream a
estreamDeg = estreamLiftA2 S.streamDeg

estreamSeq :: Seq (EStream a) -> EStream a
estreamSeq = estreamRel . fmap (,1)

estreamRel :: Seq (EStream a, Rational) -> EStream a
estreamRel = EStream . fmap S.streamRel . traverse (\(EStream e, r) -> fmap (,r) e)

estreamRep :: Integer -> EStream a -> EStream a
estreamRep = estreamMap . S.streamRep

estreamCont :: Integer -> (CycleTime -> a) -> EStream a
estreamCont sr = EStream . Right . S.streamCont sr

estreamEuc :: Euclid -> EStream a -> EStream a
estreamEuc = estreamMap . S.streamEuc

estreamRand :: Seq (EStream a) -> EStream a
estreamRand = EStream . fmap S.streamRand . traverse unEStream

estreamAlt :: Seq (EStream a) -> EStream a
estreamAlt = EStream . fmap S.streamAlt . traverse unEStream

estreamPar :: Seq (EStream a) -> EStream a
estreamPar = EStream . fmap S.streamPar . traverse unEStream

estreamSwitch :: EStream a -> CycleTime -> EStream a -> EStream a
estreamSwitch e1 t = estreamLiftA2 (`S.streamSwitch` t) e1

estreamPieces :: EStream a -> Seq (CycleTime, EStream a) -> EStream a
estreamPieces e1 = EStream . liftA2 S.streamPieces (unEStream e1) . traverse (\(t, EStream e) -> fmap (t,) e)

instance Pattern EStream where
  type PatM EStream = Identity
  type PatA EStream = ()
  patCon' = const . runIdentity
  patPure' = Identity . pure
  patEmpty' = Identity mempty
  patPar' = Identity . estreamPar
  patAlt' = Identity . estreamAlt
  patRand' = Identity . estreamRand
  patSeq' = Identity . estreamSeq
  patRel' = Identity . estreamRel
  patEuc' e = Identity . estreamEuc e
  patRep' r = Identity . estreamRep r
  patFast' p = Identity . estreamFast p
  patSlow' p = Identity . estreamSlow p
  patFastBy' r = Identity . estreamFastBy r
  patSlowBy' r = Identity . estreamSlowBy r
  patDeg' p = Identity . estreamDeg p
  patDegBy' r = Identity . estreamDegBy r

instance PatternUnwrap b EStream where
  patUnwrap' = const . runIdentity

instance Flow EStream where
  flowApply = estreamApply
  flowFilter = estreamFilter
  flowEarlyBy = estreamEarlyBy
  flowLateBy = estreamLateBy
  flowEarly = estreamEarly
  flowLate = estreamLate
  flowSwitch = estreamSwitch
  flowPieces = estreamPieces
