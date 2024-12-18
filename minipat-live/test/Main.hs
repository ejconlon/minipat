{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import Control.Monad.IO.Class (liftIO)
import Dahdit.Midi.Osc (Datum (..))
import Data.Default (def)
import Data.Ratio ((%))
import Data.Sequence qualified as Seq
import Minipat.Live.Attrs (attrsSingleton)
import Minipat.Live.Backend (PlayMeta (..), WithPlayMeta (..))
import Minipat.Live.Core (Env (..), mergeRecord, setOrbit)
import Minipat.Live.Extra (parseSound)
import Minipat.Time (arcStart, bpmToCps)
import Nanotime (PosixTime, addTime, timeDeltaFromFracSecs)
import PropUnit (TestTree, testGroup, testMain, testUnit, (===))

data R a = R !Integer !PosixTime !a
  deriving stock (Eq, Ord, Show)

projectR :: WithPlayMeta a -> R a
projectR (WithPlayMeta pm a) = R (pmOrbit pm) (arcStart (pmRealArc pm)) a

testRecordCase :: (Integer, Integer) -> TestTree
testRecordCase (tempo, gpc) =
  let cps = bpmToCps 4 (fromInteger tempo)
  in  testUnit (show tempo ++ " " ++ show gpc) $ do
        let env = def {envCps = cps, envGpc = gpc}
            cycStart = 0
            cycEnd = 2
            realStart = 0
            ahead = timeDeltaFromFracSecs (1 / (cps * fromInteger gpc))
            offset = addTime realStart ahead
            time c = addTime offset (timeDeltaFromFracSecs (c / cps))
            expectedEvs =
              Seq.fromList
                [ R 1 (time 0) (attrsSingleton "sound" (DatumString "bd"))
                , R 1 (time (1 % 2)) (attrsSingleton "sound" (DatumString "sd"))
                , R 1 (time 1) (attrsSingleton "sound" (DatumString "bd"))
                , R 1 (time (3 % 2)) (attrsSingleton "sound" (DatumString "sd"))
                ]
            expectedEnd = addTime (time (fromInteger cycEnd)) (negate ahead)
        (actualEvs, actualEnd) <- liftIO $ mergeRecord env cycStart cycEnd realStart $ \st ->
          setOrbit st 1 (parseSound "bd sd")
        let actualEvs' = fmap projectR actualEvs
        actualEvs' === expectedEvs
        actualEnd === expectedEnd

testRecord :: TestTree
testRecord =
  testGroup "record" $
    fmap
      testRecordCase
      [ (120, 1)
      , (120, 2)
      , (120, 4)
      ]

main :: IO ()
main =
  testMain $ \_ ->
    testGroup
      "minipat-Live"
      [ testRecord
      ]
