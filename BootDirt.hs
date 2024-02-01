:set -fno-warn-name-shadowing
:set -XOverloadedStrings
:set prompt "> "
:set prompt-cont "| "

import Data.Ratio ((%))
import Minipat.Dirt.Core qualified as C
import Minipat.Dirt.Logger qualified as L
import Minipat.Dirt.Prelude

logger <- L.newLogger

L.logInfo logger "Initializing"

st <- C.initSt logger C.defaultEnv

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

d0 = setOrbit 0
d1 = setOrbit 1
d2 = setOrbit 2
d3 = setOrbit 3
d4 = setOrbit 4
d5 = setOrbit 5
d6 = setOrbit 6
d7 = setOrbit 7

setPlaying True

L.logInfo logger "Ready!"
