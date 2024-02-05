{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Test where

import Control.Concurrent.MVar (withMVar)
import Control.Exception (throwIO)
import Dahdit.Midi.Osc (Datum (..))
import Data.Foldable (for_)
import Data.Ratio ((%))
import Data.Sequence (Seq)
import Minipat.Dirt.Core
  ( OscConn
  , Resources (..)
  , St (..)
  , recvPacket
  , sendPacket
  , setOrbit
  , setPlaying
  , setTempo
  , withSt
  )
import Minipat.Dirt.Osc (Attrs, PlayEnv (..), PlayErr, Timed (..), attrs, convertTape, handshakePacket, playPacket)
import Minipat.Stream (Ev (..), streamFastBy, tapeSingleton)
import Minipat.Time (Arc (..), Span (..))
import Nanotime (TimeLike (..), threadDelayDelta, timeDeltaFromFracSecs)

sendHandshake :: OscConn -> IO ()
sendHandshake conn = sendPacket conn handshakePacket

sendPlay :: OscConn -> Either PlayErr (Seq (Timed Attrs)) -> IO ()
sendPlay conn mpevs =
  case mpevs of
    Left err -> throwIO err
    Right pevs ->
      for_ pevs $ \pev -> do
        let tp@(Timed tm pkt) = fmap playPacket pev
        print tp
        now <- currentTime
        threadDelayDelta (diffTime now tm)
        sendPacket conn pkt

testHandshake :: IO ()
testHandshake = do
  putStrLn "handshake - initializing"
  withSt $ \st -> do
    putStrLn "sending handshake"
    withMVar (stRes st) (sendHandshake . resConn)
    putStrLn "listening"
    resp <- recvPacket st
    putStrLn "received"
    print resp

testPlay :: IO ()
testPlay = do
  putStrLn "play - initializing"
  withSt $ \st -> do
    dawn <- currentTime
    putStrLn ("sending play @ " <> show dawn)
    let cps = 1 % 2
        penv = PlayEnv dawn 0 cps
        arg =
          convertTape penv $
            tapeSingleton $
              Ev (Span (Arc 0 1) (Just (Arc 0 1))) $
                attrs
                  [ ("sound", DatumString "tabla")
                  , ("orbit", DatumInt32 0)
                  ]
    withMVar (stRes st) (\res -> sendPlay (resConn res) arg)
    putStrLn "done"

testReal :: IO ()
testReal = do
  putStrLn "real - initializing"
  withSt $ \st -> do
    withMVar (stRes st) (sendHandshake . resConn)
    let m =
          attrs
            [ ("sound", DatumString "cpu")
            , ("orbit", DatumInt32 0)
            ]
    setOrbit st 0 (streamFastBy 4 (pure m))
    setPlaying st True
    threadDelayDelta (timeDeltaFromFracSecs @Double 6)
    setTempo st 180
    threadDelayDelta (timeDeltaFromFracSecs @Double 6)
    setPlaying st False
