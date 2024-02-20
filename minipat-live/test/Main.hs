{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import Dahdit.Midi.Osc (Datum (..))
import Data.Sequence qualified as Seq
import Minipat.Live.Attrs (attrsFromList)
import Minipat.Live.Combinators (sound)
import Minipat.Live.Core (setOrbit, simpleRecord)
import Minipat.Live.Resources (Timed (..))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testRecord :: TestTree
testRecord = testCase "record" $ do
  let expected =
        Seq.fromList
          [ Timed 2000000000 $
              attrsFromList
                [ ("cps", DatumFloat 0.5)
                , ("delta", DatumFloat 1000000.0)
                , ("orbit", DatumInt32 1)
                , ("sound", DatumString "bd")
                ]
          , Timed 3000000000 $
              attrsFromList
                [ ("cps", DatumFloat 0.5)
                , ("delta", DatumFloat 1000000.0)
                , ("orbit", DatumInt32 1)
                , ("sound", DatumString "sd")
                ]
          ]
  actual <- simpleRecord $ \st ->
    setOrbit st 1 (sound "bd sd")
  actual @?= expected

main :: IO ()
main =
  defaultMain $
    testGroup
      "minipat-Live"
      [ testRecord
      ]
