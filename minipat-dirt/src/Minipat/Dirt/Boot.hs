{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Boot where

import Minipat.Dirt.Attrs (Attrs, IsAttrs (..))
import Minipat.Dirt.Core qualified as C
import Minipat.Dirt.Logger qualified as L
import Minipat.EStream (EStream)
import Minipat.Stream (Stream)
import Nanotime (TimeDelta)
import Prettyprinter (Pretty)

class Dirt where
  dirt :: C.St

initialize :: IO C.St
initialize = do
  logger <- L.newLogger
  L.logInfo logger "Initializing"
  C.initSt logger C.defaultEnv

dispose :: (Dirt) => IO ()
dispose = C.disposeSt dirt

getDebug :: (Dirt) => IO Bool
getDebug = C.getDebug dirt

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

setDebug :: (Dirt) => Bool -> IO ()
setDebug = C.setDebug dirt

setCps :: (Dirt) => Rational -> IO ()
setCps = C.setCps dirt

setPlaying :: (Dirt) => Bool -> IO ()
setPlaying = C.setPlaying dirt

setCycle :: (Dirt) => Integer -> IO ()
setCycle = C.setCycle dirt

setTempo :: (Dirt) => Rational -> IO ()
setTempo = C.setTempo dirt

setOrbit :: (Dirt) => Integer -> EStream Attrs -> IO ()
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

checkTasks :: (Dirt) => IO ()
checkTasks = C.checkTasks dirt

peek :: (Dirt, Pretty a) => EStream a -> IO ()
peek = C.peek dirt

d :: (Dirt, IsAttrs a) => Integer -> EStream a -> IO ()
d i = setOrbit i . fmap toAttrs

d0, d1, d2, d3, d4, d5, d6, d7 :: (Dirt) => (IsAttrs a) => EStream a -> IO ()
d0 = d 0
d1 = d 1
d2 = d 2
d3 = d 3
d4 = d 4
d5 = d 5
d6 = d 6
d7 = d 7
