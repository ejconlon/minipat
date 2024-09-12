-- | Controls and prelude for the live system. `Boot` modules are
-- generally intended to be imported unqualified into the REPL to
-- provide full functionality.
module Minipat.Live.Boot
  ( LiveSt (..)
  , readLiveSt
  , allocate
  , allocateWith
  , initialize
  , reallocate
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
  , status
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
  )
where

import Control.Monad (void)
import Data.Default (Default (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Minipat.EStream (EStream)
import Minipat.Live.Attrs (Attrs, IsAttrs)
import Minipat.Live.Backend qualified as B
import Minipat.Live.Combinators
import Minipat.Live.Core qualified as C
import Minipat.Live.Logger qualified as L
import Minipat.Live.Play (WithOrbit)
import Minipat.Stream (Stream)
import Nanotime (TimeDelta)
import Prettyprinter (Pretty)

class (B.Backend LiveBackend) => LiveSt where
  type LiveBackend :: Type
  liveStRef :: IORef (C.St LiveBackend)

readLiveSt :: (LiveSt) => IO (C.St LiveBackend)
readLiveSt = readIORef liveStRef

allocate :: (Default i) => IO (IORef (C.St i))
allocate = allocateWith def

allocateWith :: i -> IO (IORef (C.St i))
allocateWith be = do
  logger <- L.newLogger
  st <- C.newSt logger be def
  newIORef st

initialize :: (LiveSt) => IO ()
initialize = readLiveSt >>= C.initRes False

reallocate :: (LiveSt) => LiveBackend -> C.Env -> IO ()
reallocate be env = do
  oldSt <- readLiveSt
  C.disposeSt oldSt
  newSt <- C.newSt (C.stLogger oldSt) be env
  writeIORef liveStRef newSt

dispose :: (LiveSt) => IO ()
dispose = readLiveSt >>= C.disposeSt

getCps :: (LiveSt) => IO Rational
getCps = readLiveSt >>= C.getCps

getAhead :: (LiveSt) => IO TimeDelta
getAhead = readLiveSt >>= C.getAhead

getPlaying :: (LiveSt) => IO Bool
getPlaying = readLiveSt >>= C.getPlaying

getStream :: (LiveSt) => IO (Stream (WithOrbit Attrs))
getStream = readLiveSt >>= C.getStream

getCycle :: (LiveSt) => IO Integer
getCycle = readLiveSt >>= C.getCycle

getTempo :: (LiveSt) => IO Rational
getTempo = readLiveSt >>= C.getTempo

setCps :: (LiveSt) => Rational -> IO ()
setCps x = readLiveSt >>= \st -> C.setCps st x

setPlaying :: (LiveSt) => Bool -> IO ()
setPlaying x = readLiveSt >>= \st -> C.setPlaying st x

setCycle :: (LiveSt) => Integer -> IO ()
setCycle x = readLiveSt >>= \st -> C.setCycle st x

setTempo :: (LiveSt) => Rational -> IO ()
setTempo x = readLiveSt >>= \st -> C.setTempo st x

setOrbit :: (LiveSt, IsAttrs a) => Integer -> EStream a -> IO ()
setOrbit x y = readLiveSt >>= \st -> C.setOrbit st x y

clearOrbit :: (LiveSt) => Integer -> IO ()
clearOrbit x = readLiveSt >>= \st -> C.clearOrbit st x

clearAllOrbits :: (LiveSt) => IO ()
clearAllOrbits = readLiveSt >>= C.clearAllOrbits

hush :: (LiveSt) => IO ()
hush = readLiveSt >>= C.hush

panic :: (LiveSt) => IO ()
panic = readLiveSt >>= C.panic

play :: (LiveSt) => IO ()
play = setPlaying True

stop :: (LiveSt) => IO ()
stop = setPlaying False

status :: (LiveSt) => IO ()
status = readLiveSt >>= void . C.status

checkTasks :: (LiveSt) => IO ()
checkTasks = readLiveSt >>= void . C.checkTasks

-- | Prints the stream's events that would be generated in the current cycle
peek :: (LiveSt, Pretty a) => EStream a -> IO ()
peek x = readLiveSt >>= \st -> C.peek st x

d :: (LiveSt, IsAttrs a) => Integer -> EStream a -> IO ()
d = setOrbit

d0, d1, d2, d3, d4, d5, d6, d7 :: (LiveSt, IsAttrs a) => EStream a -> IO ()
d0 = d 0
d1 = d 1
d2 = d 2
d3 = d 3
d4 = d 4
d5 = d 5
d6 = d 6
d7 = d 7
