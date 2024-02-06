{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Boot where

import Minipat.Dirt.Core qualified as C
import Minipat.Dirt.Logger qualified as L
import Minipat.Dirt.Osc (Attrs)
import Minipat.Stream (Stream)
import Nanotime (TimeDelta)

class Dirt where
  dirt :: C.St

initialize :: IO C.St
initialize = do
  logger <- L.newLogger
  L.logInfo logger "Initializing"
  C.initSt logger C.defaultEnv

dispose :: Dirt => IO ()
dispose = C.disposeSt dirt

getCps :: (Dirt) => IO Rational
getCps = C.getCps dirt

getAhead :: (Dirt) => IO TimeDelta
getAhead = C.getAhead dirt

getPlaying :: (Dirt) => IO Bool
getPlaying = C.getPlaying dirt

getStream :: (Dirt) => IO (Stream Attrs)
getStream = C.getStream dirt

getCycle :: (Dirt) => IO Integer
getCycle = C.getCycle dirt

getTempo :: (Dirt) => IO Rational
getTempo = C.getTempo dirt

setCps :: (Dirt) => Rational -> IO ()
setCps = C.setCps dirt

setPlaying :: (Dirt) => Bool -> IO ()
setPlaying = C.setPlaying dirt

setCycle :: (Dirt) => Integer -> IO ()
setCycle = C.setCycle dirt

setTempo :: (Dirt) => Rational -> IO ()
setTempo = C.setTempo dirt

setOrbit :: (Dirt) => Integer -> Stream Attrs -> IO ()
setOrbit = C.setOrbit dirt

clearOrbit :: (Dirt) => Integer -> IO ()
clearOrbit = C.clearOrbit dirt

clearAllOrbits :: (Dirt) => IO ()
clearAllOrbits = C.clearAllOrbits dirt

hush :: (Dirt) => IO ()
hush = C.hush dirt

panic :: (Dirt) => IO ()
panic = C.panic dirt

play :: (Dirt) => IO ()
play = setPlaying True

stop :: (Dirt) => IO ()
stop = setPlaying False

handshake :: (Dirt) => IO ()
handshake = C.handshake dirt

peek :: (Dirt, Show a) => Stream a -> IO ()
peek = C.peek dirt

d0, d1, d2, d3, d4, d5, d6, d7 :: (Dirt) => Stream Attrs -> IO ()
d0 = setOrbit 0
d1 = setOrbit 1
d2 = setOrbit 2
d3 = setOrbit 3
d4 = setOrbit 4
d5 = setOrbit 5
d6 = setOrbit 6
d7 = setOrbit 7
