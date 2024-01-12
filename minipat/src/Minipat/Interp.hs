module Minipat.Interp
  ( interp
  )
where

import Bowtie (jotCataM)
import Control.Applicative (Alternative (..))
import Control.Exception (Exception)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.Reader (ReaderT)
import Data.Foldable (foldMap')
import Data.Foldable1 (fold1, foldl1')
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Typeable (Typeable)
import Minipat.Ast qualified as A
import Minipat.Base qualified as B
import Minipat.Rand qualified as R

data Err b = ErrTime
  deriving stock (Eq, Ord, Show)

instance (Show b, Typeable b) => Exception (Err b)

type M b = ReaderT (A.Expansion b) (Except (Err b))

subInterp :: A.NPatX b a (B.Pat a) -> M b (B.Pat a)
subInterp = \case
  A.PatPure a -> pure (pure a)
  A.PatSilence -> pure empty
  A.PatTime _ -> throwError ErrTime
  A.PatGroup (A.GroupPat _ ty els) -> pure $
    case ty of
      A.GroupPatTypeSeq _ -> fold1 els
      A.GroupPatTypePar -> foldl1' (<|>) els
      A.GroupPatTypeRand ->
        let l = NESeq.length els
            f arc' =
              let s = R.arcSeed arc'
                  i = R.randInt l s
                  el = NESeq.index els i
              in  B.unPat el arc'
        in  B.Pat (foldMap' (f . B.spanActive) . B.spanSplit)
      A.GroupPatTypeAlt ->
        let l = NESeq.length els
            f arc' =
              let i = mod (fromInteger (B.timeFloor (B.arcStart arc'))) l
                  el = NESeq.index els i
              in  B.unPat el arc'
        in  B.Pat (foldMap' (f . B.spanActive) . B.spanSplit)
  _ -> undefined

interp :: A.NPat b a -> Either (Err b) (B.Pat a)
interp = runExcept . jotCataM subInterp . A.unPat
