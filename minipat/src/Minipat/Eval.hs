module Minipat.Eval where

import Control.Exception (SomeException (..))
import Data.Text (Text)
import Looksee (parse)
import Minipat.Ast qualified as A
import Minipat.Base qualified as B
import Minipat.Interp (Sel, SelFn, interpPat, noSelFn, yesSelFn)
import Minipat.Norm (normPat)
import Minipat.Parser (P, topPatP)

evalPat :: SelFn A.Factor A.Factor -> SelFn a c -> P a -> Text -> Either SomeException (B.Pat c)
evalPat g h p t = do
  pat <- either (Left . SomeException) Right (parse (topPatP p) t)
  let pat' = normPat pat
  either (Left . SomeException) Right (interpPat g h pat')
