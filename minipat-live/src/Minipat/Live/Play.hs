{-# LANGUAGE OverloadedStrings #-}

module Minipat.Live.Play
  ( PlayErr (..)
  , PlayMeta (..)
  , pmCycleLength
  , pmRealLength
  , WithOrbit (..)
  , WithPlayMeta (..)
  , attrsConvert
  , PlayEnv (..)
  , playEvent
  , playTape
  )
where

import Control.Exception (Exception)
import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Dahdit.Midi.Osc (Datum (..))
import Data.Functor ((<&>))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Minipat.Live.Attrs (Attrs, attrsDefault, attrsDelete, attrsInsert, attrsLookup)
import Minipat.Stream (Ev (..), Tape, tapeToList)
import Minipat.Time
  ( Arc (..)
  , CycleArc
  , CycleDelta (..)
  , CycleSpan
  , CycleTime (..)
  , PosixArc
  , arcLength
  , spanActiveStart
  , spanWholeLength
  )
import Nanotime (PosixTime, TimeDelta (..), addTime, timeDeltaFromFracSecs, timeDeltaToNanos)
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as P

data PlayErr
  = -- | Error when playing continuous signals
    PlayErrCont
  deriving stock (Eq, Ord, Show)

instance Exception PlayErr

data PlayMeta = PlayMeta
  { pmOrbit :: !Integer
  , pmRealArc :: !PosixArc
  , pmCycleArc :: !CycleArc
  , pmCps :: !Rational
  }
  deriving stock (Eq, Ord, Show)

instance Pretty PlayMeta where
  pretty pm = P.hcat [pretty (pmCycleArc pm), " d", pretty (pmOrbit pm)]

pmCycleLength :: PlayMeta -> CycleDelta
pmCycleLength = arcLength . pmCycleArc

pmRealLength :: PlayMeta -> TimeDelta
pmRealLength = arcLength . pmRealArc

data WithOrbit a = WithOrbit !Integer !a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty a) => Pretty (WithOrbit a) where
  pretty (WithOrbit o a) = P.hcat ["d", pretty o, " ", pretty a]

data WithPlayMeta a = WithPlayMeta !PlayMeta !a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty a) => Pretty (WithPlayMeta a) where
  pretty (WithPlayMeta pm a) = P.hsep [pretty pm, pretty a]

attrsConvert :: [(Text, Text)] -> WithPlayMeta Attrs -> Either Text Attrs
attrsConvert aliases (WithPlayMeta pm attrs) = do
  let delta = timeDeltaToMicros (pmRealLength pm)
      cps = realToFrac (pmCps pm)
      orbit = pmOrbit pm
  attrsUnalias aliases attrs
    >>= attrsTryInsert "delta" (DatumFloat delta)
    >>= attrsTryInsert "cps" (DatumFloat cps)
    <&> attrsDefault "orbit" (DatumInt32 (fromInteger orbit))

attrsTryInsert :: Text -> Datum -> Attrs -> Either Text Attrs
attrsTryInsert k v m =
  case attrsLookup k m of
    Nothing -> Right (attrsInsert k v m)
    Just _ -> Left ("Duplicate key: " <> k)

attrsUnalias :: [(Text, Text)] -> Attrs -> Either Text Attrs
attrsUnalias as m0 = foldM go m0 as
 where
  go !m (x, y) = do
    case attrsLookup x m of
      Nothing -> pure m
      Just v -> attrsTryInsert y v (attrsDelete x m)

spanDeltaM :: CycleSpan -> Either PlayErr CycleDelta
spanDeltaM = maybe (throwError PlayErrCont) pure . spanWholeLength

data PlayEnv = PlayEnv
  { peRealOrigin :: !PosixTime
  , peCycleBounds :: !CycleArc
  , peCps :: !Rational
  }
  deriving stock (Eq, Ord, Show)

timeDeltaToMicros :: TimeDelta -> Float
timeDeltaToMicros td =
  let (_, ns) = timeDeltaToNanos td
  in  fromIntegral ns / 1000

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
