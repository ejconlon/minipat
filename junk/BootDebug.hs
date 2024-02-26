:set -fno-warn-name-shadowing
:set -fno-warn-orphans
:set -XOverloadedLists
:set -XOverloadedStrings
:set prompt "> "
:set prompt-cont "| "

import Control.Exception
import Data.Ratio
import Minipat.Ast
import Minipat.Classes
import Minipat.Stream
import Minipat.Interp
import Minipat.Eval
import Minipat.Parser
import Minipat.Print
import Minipat.Time

let run n s = prettyPrintAll "\n" (tapeToList (streamRun (s :: Stream Ident) (Arc 0 (fromInteger n))))

let eval t = either throwIO pure (evalPat identP t) :: IO (Stream Ident)
