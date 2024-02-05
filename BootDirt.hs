:set -fno-warn-name-shadowing
:set -XOverloadedLists
:set -XOverloadedStrings
:set prompt "> "
:set prompt-cont "| "

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Minipat.Dirt.Core qualified as C
import Minipat.Dirt.Logger qualified as L
import Minipat.Dirt.Prelude

logger <- L.newLogger

L.logInfo logger "Initializing"

st <- C.initSt logger C.defaultEnv

handshake = C.handshake st

getCps = C.getCps st
getAhead = C.getAhead st
getPlaying = C.getPlaying st
getStream = C.getStream st
getCycle = C.getCycle st
getTempo = C.getTempo st

setCps = C.setCps st
setPlaying = C.setPlaying st
setCycle = C.setCycle st
setTempo = C.setTempo st
setOrbit = C.setOrbit st
clearOrbit = C.setOrbit st
clearAllOrbits = C.clearAllOrbits st
hush = C.hush st
panic = C.panic st
play = setPlaying True
stop = setPlaying False

d0 = setOrbit 0
d1 = setOrbit 1
d2 = setOrbit 2
d3 = setOrbit 3
d4 = setOrbit 4
d5 = setOrbit 5
d6 = setOrbit 6
d7 = setOrbit 7

L.logInfo logger "Handshaking"

handshake >>= \ok -> if ok then L.logInfo logger "Ready" else L.logError logger "Handshake failed"

