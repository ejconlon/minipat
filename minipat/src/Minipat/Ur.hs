module Minipat.Ur where

import Minipat.Ast (Ident)
import Minipat.Stream (Stream)
import Minipat.Time (CycleDelta)

ur
  :: CycleDelta
  -> Stream Ident
  -> [(Ident, Stream a)]
  -> [(Ident, Stream a -> Stream a)]
  -> Stream a
ur = error "TODO"
