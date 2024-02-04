-- | Taking textual patterns all the way to streams
module Minipat.Eval
  ( EvalEnv (..)
  , evalPat
  )
where

import Control.Exception (SomeException (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Looksee (parse)
import Minipat.Class (Pattern)
import Minipat.Interp (InterpEnv, interpPat)
import Minipat.Norm (normPat)
import Minipat.Parser (P, topPatP)

data EvalEnv e a where
  EvalEnv :: InterpEnv e z a -> P z -> EvalEnv e a

-- | The canonical way to parse, normalize, and interpret patterns as streams
evalPat :: (Pattern f, Show e, Typeable e) => EvalEnv e a -> Text -> Either SomeException (f a)
evalPat (EvalEnv ie p) t = do
  pat <- either (Left . SomeException) Right (parse (topPatP p) t)
  let pat' = normPat pat
  either (Left . SomeException) Right (interpPat ie pat')
