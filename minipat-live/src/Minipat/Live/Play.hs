{-# LANGUAGE OverloadedStrings #-}

module Minipat.Live.Play
  ( PlayErr (..)
  , WithOrbit (..)
  , PlayEnv (..)
  , playEvent
  , playTape
  )
where

import Control.Exception (Exception)
import Control.Monad.Except (throwError)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Minipat.Live.Backend (PlayMeta (..), WithPlayMeta (..))
import Minipat.Stream (Ev (..), Tape, tapeToList)
import Minipat.Time
  ( Arc (..)
  , CycleArc
  , CycleDelta (..)
  , CycleSpan
  , CycleTime (..)
  , spanActiveStart
  , spanWholeLength
  )
import Nanotime (PosixTime, addTime, timeDeltaFromFracSecs)
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as P

data PlayErr
  = -- | Error when playing continuous signals
    PlayErrCont
  deriving stock (Eq, Ord, Show)

instance Exception PlayErr

data WithOrbit a = WithOrbit !Integer !a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty a) => Pretty (WithOrbit a) where
  pretty (WithOrbit o a) = P.hcat ["d", pretty o, " ", pretty a]

spanDeltaM :: CycleSpan -> Either PlayErr CycleDelta
spanDeltaM = maybe (throwError PlayErrCont) pure . spanWholeLength

data PlayEnv = PlayEnv
  { peRealOrigin :: !PosixTime
  , peCycleBounds :: !CycleArc
  , peCps :: !Rational
  }
  deriving stock (Eq, Ord, Show)

playEvent
  :: PlayEnv
  -> Ev (WithOrbit q)
  -> Either PlayErr (Maybe (WithPlayMeta q))
playEvent (PlayEnv realOrigin (Arc cycleOrigin _) cps) (Ev sp (WithOrbit orbit dat)) =
  case spanActiveStart sp of
    Nothing ->
      -- Only emit start events
      Right Nothing
    Just cycleStart -> do
      let cycleOffset = cycleStart - cycleOrigin
          realStart = addTime realOrigin (timeDeltaFromFracSecs (unCycleTime cycleOffset / cps))
      cycleDelta <- spanDeltaM sp
      let cycleEnd = CycleTime (unCycleTime cycleStart + unCycleDelta cycleDelta)
          cycleArc = Arc cycleStart cycleEnd
          realLength = timeDeltaFromFracSecs (unCycleDelta cycleDelta / cps)
          realEnd = addTime realStart realLength
          realArc = Arc realStart realEnd
          pm = PlayMeta orbit realArc cycleArc cps
      pure (Just (WithPlayMeta pm dat))

traverseMaybe :: (Monad m) => (a -> m (Maybe b)) -> Seq a -> m (Seq b)
traverseMaybe f = go Empty
 where
  go !acc = \case
    Empty -> pure acc
    a :<| as' -> f a >>= maybe (go acc as') (\b -> go (acc :|> b) as')

playTape
  :: PlayEnv
  -> Tape (WithOrbit q)
  -> Either PlayErr (Seq (WithPlayMeta q))
playTape penv = traverseMaybe (playEvent penv) . Seq.fromList . tapeToList
