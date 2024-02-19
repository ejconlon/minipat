{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Boot
  ( D.DirtEnv
  , D.DirtData
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

type DirtLiveSt = (LiveSt, LiveEnv ~ D.DirtEnv, LiveData ~ D.DirtData)

initialize :: IO D.DirtSt
initialize = do
  logger <- L.newLogger
  L.logInfo logger "Initializing"
  C.initAsyncSt logger D.dirtImpl (C.defaultEnv D.defaultDirtEnv)

handshake :: (DirtLiveSt) => IO ()
handshake = D.handshake liveSt
