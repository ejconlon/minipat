-- | Taking textual patterns all the way to streams
module Minipat.Eval
  ( evalPat
  , evalPatAcc
  , evalPatForbid
  )
where

import Control.Exception (SomeException (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Looksee (parse)
import Minipat.Interp (Sel, SelAcc, accProj, accSel, forbidSel, interpPat)
import Minipat.Norm (normPat)
import Minipat.Parser (P, topPatP)
import Minipat.Stream (Stream)

-- | The canonical way to parse, normalize, and interpret patterns as streams
evalPat :: (Show e, Typeable e) => Sel e c -> (a -> c) -> P a -> Text -> Either SomeException (Stream c)
evalPat sel proj p t = do
  pat <- either (Left . SomeException) Right (parse (topPatP p) t)
  let pat' = normPat pat
  either (Left . SomeException) Right (interpPat sel proj pat')

-- | 'interpPat' acumulating selects
evalPatAcc :: P a -> Text -> Either SomeException (Stream (SelAcc a))
evalPatAcc = evalPat @Void accSel accProj

-- | 'interpPat' forbidding selects
evalPatForbid :: P a -> Text -> Either SomeException (Stream a)
evalPatForbid = evalPat @Void forbidSel id
