-- | Taking textual patterns all the way to streams
module Minipat.Eval
  ( PatternEval
  , evalPat
  , evalPatSub
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
evalPat = evalPatSub Right

-- | Same, but allows substitution before interpretation
evalPatSub :: (PatternEval f) => (a -> Either SomeException b) -> P a -> Text -> Either SomeException (f b)
evalPatSub g p t = do
  pat <- either (Left . SomeException) Right (parseNice (topPatP p) t)
  let pat' = normPat pat
  pat'' <- traverse g pat'
  either (Left . SomeException) Right (interpPat pat'')
