module Minipat.Dirt.Boot
  ( I.DirtBackend
  , I.DirtSt
  , DirtLiveSt
  , module Minipat.Dirt.Params
  , module Minipat.Live.Boot
  , module Minipat.Live.Extra
  )
where

import Minipat.Dirt.Impl qualified as I
import Minipat.Dirt.Params
import Minipat.Live.Boot
import Minipat.Live.Extra

type DirtLiveSt = (LiveSt, LiveBackend ~ I.DirtBackend)
