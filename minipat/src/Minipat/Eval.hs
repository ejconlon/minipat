-- | Taking textual patterns all the way to streams
module Minipat.Eval
  ( evalPat
  )
where

import Control.Exception (SomeException (..))
import Data.Text (Text)
import Looksee (parse)
import Minipat.Ast (Pattern)
import Minipat.Interp (interpPat)
import Minipat.Norm (normPat)
import Minipat.Parser (P, topPatP)

-- | The canonical way to parse, normalize, and interpret patterns as streams
evalPat :: (Pattern f) => P a -> Text -> Either SomeException (f a)
evalPat p t = do
  pat <- either (Left . SomeException) Right (parse (topPatP p) t)
  let pat' = normPat pat
  either (Left . SomeException) Right (interpPat pat')
