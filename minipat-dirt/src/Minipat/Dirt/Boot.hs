{-# LANGUAGE OverloadedStrings #-}

-- | Controls for the live system
module Minipat.Dirt.Boot where

import Minipat.Dirt.Attrs (Attrs, IsAttrs (..))
import Minipat.Dirt.Core qualified as C
import Minipat.Dirt.DirtCore qualified as D
import Minipat.Dirt.Logger qualified as L
import Minipat.EStream (EStream)
import Minipat.Stream (Stream)
import Nanotime (TimeDelta)
import Prettyprinter (Pretty)

class Minipat where
  minipat :: D.DirtSt

initialize :: IO D.DirtSt
initialize = do
  logger <- L.newLogger
  L.logInfo logger "Initializing"
  C.initSt logger D.dirtImpl (C.defaultEnv D.defaultDirtEnv)

dispose :: (Minipat) => IO ()
dispose = C.disposeSt minipat

getDebug :: (Minipat) => IO Bool
getDebug = C.getDebug minipat

getCps :: (Minipat) => IO Rational
getCps = C.getCps minipat

getAhead :: (Minipat) => IO TimeDelta
getAhead = C.getAhead minipat

getPlaying :: (Minipat) => IO Bool
getPlaying = C.getPlaying minipat

getStream :: (Minipat) => IO (Stream Attrs)
getStream = C.getStream minipat

getCycle :: (Minipat) => IO Integer
getCycle = C.getCycle minipat

getTempo :: (Minipat) => IO Rational
getTempo = C.getTempo minipat

setDebug :: (Minipat) => Bool -> IO ()
setDebug = C.setDebug minipat

setCps :: (Minipat) => Rational -> IO ()
setCps = C.setCps minipat

setPlaying :: (Minipat) => Bool -> IO ()
setPlaying = C.setPlaying minipat

setCycle :: (Minipat) => Integer -> IO ()
setCycle = C.setCycle minipat

setTempo :: (Minipat) => Rational -> IO ()
setTempo = C.setTempo minipat

setOrbit :: (Minipat) => Integer -> EStream Attrs -> IO ()
setOrbit = C.setOrbit minipat

clearOrbit :: (Minipat) => Integer -> IO ()
clearOrbit = C.clearOrbit minipat

clearAllOrbits :: (Minipat) => IO ()
clearAllOrbits = C.clearAllOrbits minipat

hush :: (Minipat) => IO ()
hush = C.hush minipat

panic :: (Minipat) => IO ()
panic = C.panic minipat

play :: (Minipat) => IO ()
play = setPlaying True

stop :: (Minipat) => IO ()
stop = setPlaying False

handshake :: (Minipat) => IO ()
handshake = D.handshake minipat

checkTasks :: (Minipat) => IO ()
checkTasks = C.checkTasks minipat

-- | Prints the stream's events that would be generated in the current cycle
peek :: (Minipat, Pretty a) => EStream a -> IO ()
peek = C.peek minipat

d :: (Minipat, IsAttrs a) => Integer -> EStream a -> IO ()
d i = setOrbit i . fmap toAttrs

d0, d1, d2, d3, d4, d5, d6, d7 :: (Minipat, IsAttrs a) => EStream a -> IO ()
d0 = d 0
d1 = d 1
d2 = d 2
d3 = d 3
d4 = d 4
d5 = d 5
d6 = d 6
d7 = d 7
