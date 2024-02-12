-- | Taking textual patterns all the way to streams
module Minipat.Eval
  ( PatternEval
  , evalPat
  )
where

import Control.Exception (SomeException (..))
import Data.Text (Text)
import Looksee (parse)
import Minipat.Interp (interpPat)
import Minipat.Norm (normPat)
import Minipat.Parser (Loc, P, topPatP)
import Minipat.Pattern (PatternUnwrap)

type PatternEval = PatternUnwrap Loc

-- | The canonical way to parse, normalize, and interpret patterns as streams
evalPat :: (PatternEval f) => P a -> Text -> Either SomeException (f a)
evalPat p t = do
  pat <- either (Left . SomeException) Right (parse (topPatP p) t)
  let pat' = normPat pat
  either (Left . SomeException) Right (interpPat pat')
