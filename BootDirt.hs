:set -fno-warn-name-shadowing
:set -XOverloadedStrings
:set prompt "> "
:set prompt-cont "| "

import Data.Ratio ((%))
import Minipat.Dirt.Core qualified as C
import Minipat.Dirt.Prelude

st <- C.initSt C.defaultEnv

getCps = C.getCps st
getAhead = C.getAhead st
getPlaying = C.getPlaying st
getStream = C.getStream st
getCycle = C.getCycle st
getTempo = C.getTempo st

setCps = C.setCps st
setPlaying = C.setPlaying st
setStream = C.setStream st
setCycle = C.setCycle st
setTempo = C.setTempo st

setPlaying True
