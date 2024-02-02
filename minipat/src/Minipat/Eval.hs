-- | Taking textual patterns all the way to streams
module Minipat.Eval
  ( evalPat
  , evalPatAccSel
  , evalPatNoSel
  )
where

import Control.Exception (SomeException (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Looksee (parse)
import Minipat.Interp (SelAcc, SelFn, accSelFn, accSelProj, interpPat, noSelFn)
import Minipat.Norm (normPat)
import Minipat.Parser (P, topPatP)
import Minipat.Stream (Stream)

-- | The canonical way to parse, normalize, and interpret patterns as streams
evalPat :: (Show e, Typeable e) => SelFn e c -> (a -> c) -> P a -> Text -> Either SomeException (Stream c)
evalPat selFn projFn p t = do
  pat <- either (Left . SomeException) Right (parse (topPatP p) t)
  let pat' = normPat pat
  either (Left . SomeException) Right (interpPat selFn projFn pat')

-- | 'interpPat' acumulating selects
evalPatAccSel :: P a -> Text -> Either SomeException (Stream (SelAcc a))
evalPatAccSel = evalPat @Void accSelFn accSelProj

-- | 'interpPat' forbidding selects
evalPatNoSel :: P a -> Text -> Either SomeException (Stream a)
evalPatNoSel = evalPat @Void noSelFn id
