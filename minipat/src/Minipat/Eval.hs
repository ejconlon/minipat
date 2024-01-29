-- | Taking textual patterns all the way to streams
module Minipat.Eval where

import Control.Exception (SomeException (..))
import Data.Text (Text)
import Looksee (parse)
import Minipat.Ast (Factor)
import Minipat.Interp (SelFn, interpPat)
import Minipat.Norm (normPat)
import Minipat.Parser (P, topPatP)
import Minipat.Stream (Stream)

-- | The canonical way to parse, normalize, and interpret patterns as streams
evalPat :: SelFn Factor Factor -> SelFn a c -> P a -> Text -> Either SomeException (Stream c)
evalPat g h p t = do
  pat <- either (Left . SomeException) Right (parse (topPatP p) t)
  let pat' = normPat pat
  either (Left . SomeException) Right (interpPat g h pat')
