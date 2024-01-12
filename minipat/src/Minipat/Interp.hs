module Minipat.Interp where

import Bowtie (jotRight)
import Minipat.Ast qualified as A
import Minipat.Types qualified as T

interp :: A.NPat b a -> T.Pat a
interp = jotRight go . A.unPat
 where
  go = \case
    _ -> error "TODO"
