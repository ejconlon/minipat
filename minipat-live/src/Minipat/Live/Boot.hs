-- | Controls and prelude for the live system
module Minipat.Live.Boot
  ( LiveSt (..)
  , dispose
  , getCps
  , getAhead
  , getPlaying
  , getStream
  , getCycle
  , getTempo
  , setCps
  , setPlaying
  , setCycle
  , setTempo
  , setOrbit
  , clearOrbit
  , clearAllOrbits
  , hush
  , panic
  , play
  , stop
  , checkTasks
  , peek
  , d
  , d0
  , d1
  , d2
  , d3
  , d4
  , d5
  , d6
  , d7
  , module Minipat.Live.Combinators
  , module Minipat.Live.Params
  )
where

import Data.Kind (Type)
import Minipat.EStream (EStream)
import Minipat.Live.Attrs (Squishy (..))
import Minipat.Live.Backend qualified as B
import Minipat.Live.Combinators
import Minipat.Live.Core qualified as C
import Minipat.Live.Params
import Minipat.Live.Play (WithOrbit)
import Minipat.Stream (Stream)
import Nanotime (TimeDelta)
import Prettyprinter (Pretty)

class (B.Backend LiveBackend) => LiveSt where
  type LiveBackend :: Type
  liveSt :: C.St LiveBackend

type LiveAttrs = B.BackendAttrs LiveBackend

dispose :: (LiveSt) => IO ()
dispose = C.disposeSt liveSt

getCps :: (LiveSt) => IO Rational
getCps = C.getCps liveSt

getAhead :: (LiveSt) => IO TimeDelta
getAhead = C.getAhead liveSt

getPlaying :: (LiveSt) => IO Bool
getPlaying = C.getPlaying liveSt

getStream :: (LiveSt) => IO (Stream (WithOrbit LiveAttrs))
getStream = C.getStream liveSt

getCycle :: (LiveSt) => IO Integer
getCycle = C.getCycle liveSt

getTempo :: (LiveSt) => IO Rational
getTempo = C.getTempo liveSt

setCps :: (LiveSt) => Rational -> IO ()
setCps = C.setCps liveSt

setPlaying :: (LiveSt) => Bool -> IO ()
setPlaying = C.setPlaying liveSt

setCycle :: (LiveSt) => Integer -> IO ()
setCycle = C.setCycle liveSt

setTempo :: (LiveSt) => Rational -> IO ()
setTempo = C.setTempo liveSt

setOrbit :: (LiveSt, Squishy LiveAttrs a) => Integer -> EStream a -> IO ()
setOrbit = C.setOrbit liveSt

clearOrbit :: (LiveSt) => Integer -> IO ()
clearOrbit = C.clearOrbit liveSt

clearAllOrbits :: (LiveSt) => IO ()
clearAllOrbits = C.clearAllOrbits liveSt

hush :: (LiveSt) => IO ()
hush = C.hush liveSt

panic :: (LiveSt) => IO ()
panic = C.panic liveSt

play :: (LiveSt) => IO ()
play = setPlaying True

stop :: (LiveSt) => IO ()
stop = setPlaying False

checkTasks :: (LiveSt) => IO ()
checkTasks = C.checkTasks liveSt

-- | Prints the stream's events that would be generated in the current cycle
peek :: (LiveSt, Pretty a) => EStream a -> IO ()
peek = C.peek liveSt

d :: (LiveSt, Squishy LiveAttrs a) => Integer -> EStream a -> IO ()
d = setOrbit

d0, d1, d2, d3, d4, d5, d6, d7 :: (LiveSt, Squishy LiveAttrs a) => EStream a -> IO ()
d0 = d 0
d1 = d 1
d2 = d 2
d3 = d 3
d4 = d 4
d5 = d 5
d6 = d 6
d7 = d 7
