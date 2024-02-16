-- | Taking textual patterns all the way to streams
module Minipat.Eval
  ( PatternEval
  , evalPat
  )
where

import Control.Exception (SomeException (..))
import Data.Text (Text)
import Minipat.Classes (PatternUnwrap)
import Minipat.Interp (interpPat)
import Minipat.Norm (normPat)
import Minipat.Parser (Loc, P, parseNice, topPatP)

type PatternEval = PatternUnwrap Loc

-- | The canonical way to parse, normalize, and interpret patterns as streams
evalPat :: (PatternEval f) => P a -> Text -> Either SomeException (f a)
evalPat p t = do
  pat <- either (Left . SomeException) Right (parseNice (topPatP p) t)
  let pat' = normPat pat
  either (Left . SomeException) Right (interpPat pat')
