module Minipat.Time where

import Nanotime (TimeDelta, timeDeltaFromFracSecs, timeDeltaToFracSecs)

-- TODO newtype this CycleTime
type Time = Rational

timeFloor :: Time -> Integer
timeFloor = floor

timeCeil :: Time -> Integer
timeCeil = (+ 1) . timeFloor

timeMidpoint :: Time -> Time -> Time
timeMidpoint s e = s + (e - s) / 2

data Arc = Arc {arcStart :: !Time, arcEnd :: !Time}
  deriving stock (Eq, Ord, Show)

arcUnion :: Arc -> Arc -> Arc
arcUnion (Arc s1 e1) (Arc s2 e2) = Arc (min s1 s2) (max e1 e2)

arcIntersect :: Arc -> Arc -> Arc
arcIntersect (Arc s1 e1) (Arc s2 e2) =
  let s3 = max s1 s2
      e3 = min e1 e2
  in  Arc s3 (max s3 e3)

arcWiden :: Arc -> Arc
arcWiden (Arc s e) = Arc (fromInteger (timeFloor s)) (fromInteger (timeCeil e))

arcMidpoint :: Arc -> Time
arcMidpoint (Arc s e) = timeMidpoint s e

arcTimeMapMono :: (Time -> Time) -> Arc -> Arc
arcTimeMapMono f (Arc s e) = Arc (f s) (f e)

arcWrap :: Arc -> Rational -> (Time, Time, Time)
arcWrap (Arc s e) w =
  if s >= w
    then
      let d = e - s
          k = w * fromInteger (floor (s / w))
          c = s - k
      in  (k, c, d)
    else (0, s, e - s)

data Span = Span
  { spanActive :: !Arc
  , spanWhole :: !(Maybe Arc)
  }
  deriving stock (Eq, Ord, Show)

spanTimeMapMono :: (Time -> Time) -> Span -> Span
spanTimeMapMono f (Span ac wh) = Span (arcTimeMapMono f ac) (fmap (arcTimeMapMono f) wh)

spanWholeMapMono :: (Maybe Arc -> Maybe Arc) -> Span -> Span
spanWholeMapMono f (Span ac wh) = Span ac (f wh)

-- | Expands the active arc to cover the whole event
spanExtent :: Span -> Span
spanExtent sp@(Span _ wh) = maybe sp (`Span` wh) wh

-- | Splits an Arc into single-cycle spans
spanSplit :: Arc -> [(Integer, Span)]
spanSplit (Arc s0 e) =
  let ef = fromInteger (timeFloor e)
      go s =
        let si = timeFloor s
            sf = fromInteger si
            sc = fromInteger (timeCeil s)
            wh = Just (Arc sf sc)
        in  if sf == ef || sc == e
              then [(si, Span (Arc s e) wh)]
              else (si, Span (Arc s sc) wh) : go sc
  in  go s0

-- | The start of the span in cycle time, if active
spanCycle :: Span -> Maybe Rational
spanCycle = \case
  sp@(Span _ (Just (Arc sw _))) | spanIsStart sp -> Just sw
  _ -> Nothing

-- | The length of the whole event in cycle time, if discrete
spanDelta :: Span -> Maybe Rational
spanDelta = \case
  Span _ (Just (Arc sw ew)) -> Just (ew - sw)
  _ -> Nothing

-- | True if active start aligns with whole start
spanIsStart :: Span -> Bool
spanIsStart (Span (Arc sa _) mwh) =
  case mwh of
    Nothing -> True
    Just (Arc sw _) -> sa == sw

-- | Convert BPM to CPS, given BPC (beats per cycle)
-- (often 4 beats/bar, 1 bar/cycle, so 4 bpc)
bpmToCps :: Integer -> Rational -> Rational
bpmToCps bpc bpm = (bpm / 60) / fromInteger bpc

-- | Convert CPS to BPM, given BPC (beats per cycle)
cpsToBpm :: Integer -> Rational -> Rational
cpsToBpm bpc cps = (cps * fromInteger bpc) * 60

-- | Given CPS convert absolute time diff from start to cycle time
deltaToCycle :: Rational -> TimeDelta -> Time
deltaToCycle cps = (cps *) . timeDeltaToFracSecs

-- | Given CPS convert cycle time to absolute time diff from start
cycleToDelta :: Rational -> Time -> TimeDelta
cycleToDelta cps = timeDeltaFromFracSecs . (/ cps)

-- | Given CPS return relative time from origin to target
relDelta :: Rational -> Rational -> Rational -> TimeDelta
relDelta cps origin target = timeDeltaFromFracSecs (cps * (target - origin))
