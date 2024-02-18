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
import Minipat.Dirt.Prelude

putStrLn "==== Minipat ==============================================="
putStrLn "Quit with Ctrl-d or `:quit`"
putStrLn "Clear stream with `hush` or stop with `panic`"
putStrLn "List available functions with `:browse Minipat.Dirt.Prelude`"
putStrLn "Show documentation with `:doc someFunctionName`"
putStrLn "============================================================"

minipatInst <- initialize

instance Minipat where minipat = minipatInst

handshake

