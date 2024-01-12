module Minipat.Proc
  ( ProcErr (..)
  , ProcM
  , partPM
  , throwPM
  , bottomUpPM
  )
where

import Bowtie (pattern JotP)
import Control.Exception (Exception)
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Data.Bitraversable (bitraverse)
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Typeable (Typeable)
import Minipat.Ast qualified as A

-- | Path components are pushed onto the end as
-- we descend the tree
data ProcErr e b = ProcErr
  { procErrPath :: !(NESeq b)
  , procErrReason :: !e
  }
  deriving stock (Eq, Ord, Show)

instance
  (Show b, Typeable b, Show e, Typeable e)
  => Exception (ProcErr b e)

data ProcEnv b c = ProcEnv
  { procEnvPath :: !(NESeq b)
  , procEnvKey :: !c
  }
  deriving stock (Eq, Ord, Show)

type ProcM e b c = ReaderT (ProcEnv b c) (Except (ProcErr e b))

runPM :: ProcM e b c a -> ProcEnv b c -> Either (ProcErr e b) a
runPM m bs = runExcept (runReaderT m bs)

partPM :: ProcM e b c c
partPM = asks procEnvKey

throwPM :: e -> ProcM e b c a
throwPM e = asks procEnvPath >>= \bs -> throwError (ProcErr bs e)

bottomUpPM :: (c -> b) -> (A.PatX c a x -> ProcM e b c x) -> A.Pat c a -> Either (ProcErr e b) x
bottomUpPM g f (A.Pat (JotP c0 p0)) = runPM (go p0) (ProcEnv (NESeq.singleton (g c0)) c0)
 where
  go p = bitraverse pure (\(JotP c1 p1) -> local (\(ProcEnv bs _) -> ProcEnv (bs NESeq.|> g c1) c1) (go p1)) p >>= f
