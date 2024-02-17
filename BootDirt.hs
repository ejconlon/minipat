:set -fno-warn-name-shadowing
:set -fno-warn-orphans
:set -XOverloadedLists
:set -XOverloadedStrings
:set prompt "> "
:set prompt-cont "| "

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Minipat.Dirt.Boot
import Minipat.Dirt.Params
import Minipat.Dirt.Prelude

dirtInst <- initialize

instance Dirt where dirt = dirtInst

handshake
