{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Boot
  ( D.DirtBackend
  , D.DirtSt
  , DirtLiveSt
  , initialize
  , handshake
  , module Minipat.Live.Boot
  )
where

import Minipat.Dirt.Impl qualified as D
import Minipat.Live.Boot
import Minipat.Live.Core qualified as C
import Minipat.Live.Logger qualified as L

type DirtLiveSt = (LiveSt, LiveBackend ~ D.DirtBackend)

initialize :: IO D.DirtSt
initialize = do
  logger <- L.newLogger
  L.logInfo logger "Initializing"
  C.initAsyncSt logger D.defaultDirtBackend C.defaultEnv

-- TODO move handshake into init
handshake :: (DirtLiveSt) => IO ()
handshake = D.handshake liveSt
