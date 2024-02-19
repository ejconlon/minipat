:set -fno-warn-name-shadowing
:set -fno-warn-orphans
:set -XOverloadedLists
:set -XOverloadedStrings
:set -XTypeFamilies
:set prompt "> "
:set prompt-cont "| "

import Minipat.Dirt.Boot

putStrLn "==== Minipat ==============================================="
putStrLn "Quit with Ctrl-d or `:quit`"
putStrLn "Clear stream with `hush` or stop with `panic`"
putStrLn "List available functions with `:browse Minipat.Dirt.Boot`"
putStrLn "Show documentation with `:doc someFunctionName`"
putStrLn "============================================================"

dirtSt <- initialize

instance LiveSt where { type LiveEnv = DirtEnv; type LiveData = DirtData; liveSt = dirtSt }

handshake

