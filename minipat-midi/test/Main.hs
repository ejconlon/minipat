{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import Control.Exception (throwIO)
import Control.Monad.ST (runST)
import Dahdit
import Data.Bits (Bits (..))
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Short qualified as BSS
import Data.Char (digitToInt)
import Minipat.Midi.Mpk
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- TODO clean up the hex bytes stuff and put it in dahdit-test

showHexBytes :: ShortByteString -> ShortByteString
showHexBytes = go mempty . BSS.unpack
 where
  go !acc = \case
    [] -> BSS.toShort (BS.toStrict (BSB.toLazyByteString acc))
    [x] -> go (acc <> BSB.word8HexFixed x) []
    x : xs -> go (acc <> BSB.word8HexFixed x <> " ") xs

readHexBytes :: ShortByteString -> ShortByteString
readHexBytes = go mempty . BSS.unpack . BSS.filter (/= 0x20)
 where
  go !acc = \case
    [] -> BSS.toShort (BS.toStrict (BSB.toLazyByteString acc))
    x : y : zs ->
      let w = shiftL (hexChar x) 4 .|. hexChar y
      in  go (acc <> BSB.word8 w) zs
    _ -> error "invalid hex bytes"
  hexChar = fromIntegral . digitToInt . BSI.w2c

testMpkRequest :: TestTree
testMpkRequest = testCase "mpk manual" $ do
  let expected = "f0 47 7f 49 66 00 01 01 f7"
      actual = showHexBytes (runST (encode (requestProgConfig (ProgBank 1))))
  actual @?= expected

testMpkSend :: TestTree
testMpkSend = testCase "mpk send" $ do
  staticByteSize @ProgConfig undefined @?= 245
  let expectedStr =
        "f0 47 7f 49 64 01 76 01 50 47 4d 3a 4d 50 43 00 00 00 00 00 00 00 00 00 09 01 00 04 00 00 04 00 00 00 03 00 78 00 00 00 00 02 01 01 24 00 10 25 01 11 26 02 12 27 03 13 28 04 14 29 05 15 2a 06 16 2b 07 17 2c 08 18 2d 09 19 2e 0a 1a 2f 0b 1b 30 0c 1c 31 0d 1d 32 0e 1e 33 0f 1f 00 46 00 7f 51 4c 49 4e 4b 35 00 00 00 00 00 00 00 00 00 00 00 47 00 7f 51 4c 49 4e 4b 36 00 00 00 00 00 00 00 00 00 00 00 48 00 7f 51 4c 49 4e 4b 37 00 00 00 00 00 00 00 00 00 00 00 49 00 7f 51 4c 49 4e 4b 38 00 00 00 00 00 00 00 00 00 00 00 4a 00 7f 51 4c 49 4e 4b 31 00 00 00 00 00 00 00 00 00 00 00 4b 00 7f 51 4c 49 4e 4b 32 00 00 00 00 00 00 00 00 00 00 00 4c 00 7f 51 4c 49 4e 4b 33 00 00 00 00 00 00 00 00 00 00 00 4d 00 7f 51 4c 49 4e 4b 34 00 00 00 00 00 00 00 00 00 00 0c f7"
      expectedBs = readHexBytes expectedStr
  showHexBytes expectedBs @?= expectedStr
  let payloadBs = BSS.take 245 (BSS.drop 8 expectedBs)
      result = runST (decode @ProgConfig payloadBs)
  cfg <- either throwIO pure (fst result)
  let encodedBs = runST (encode cfg)
  encodedBs @?= payloadBs
  let finalStr = showHexBytes (runST (encode (sendProgConfig (ProgBank 1) cfg)))
  finalStr @?= expectedStr

testMpk :: TestTree
testMpk =
  testGroup
    "mpk"
    [ testMpkRequest
    , testMpkSend
    ]

main :: IO ()
main =
  defaultMain $
    testGroup
      "minipat-midi"
      [ testMpk
      ]
