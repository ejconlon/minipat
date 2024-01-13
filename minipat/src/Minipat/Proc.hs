module Minipat.Proc
  ( ProcErr (..)
  , ProcM
  , runPM
  , askPM
  , asksPM
  , pushPM
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
  { procEnvFn :: !(c -> b)
  , procEnvPath :: !(NESeq b)
  , procEnvKey :: !c
  }

newProcEnv :: (c -> b) -> c -> ProcEnv b c
newProcEnv g c = ProcEnv g (NESeq.singleton (g c)) c

pushProcEnv :: c -> ProcEnv b c -> ProcEnv b c
pushProcEnv c (ProcEnv g bs _) = ProcEnv g (bs NESeq.|> g c) c

newtype ProcM e b c a = ProcM {unProcM :: ReaderT (ProcEnv b c) (Except (ProcErr e b)) a}
  deriving newtype (Functor, Applicative, Monad)

runPM :: ProcM e b c a -> (c -> b) -> c -> Either (ProcErr e b) a
runPM m g c = runExcept (runReaderT (unProcM m) (newProcEnv g c))

askPM :: ProcM e b c c
askPM = ProcM (asks procEnvKey)

asksPM :: (c -> d) -> ProcM e b c d
asksPM f = ProcM (asks (f . procEnvKey))

pushPM :: c -> ProcM e b c a -> ProcM e b c a
pushPM c (ProcM m) = ProcM (local (pushProcEnv c) m)

-- localPM :: (c -> c) -> ProcM e b c a -> ProcM e b c a
-- localPM f m = askPM >>= \c -> pushPM (f c) m

throwPM :: e -> ProcM e b c a
throwPM e = ProcM (asks procEnvPath) >>= \bs -> ProcM (throwError (ProcErr bs e))

bottomUpPM :: (c -> b) -> (A.PatX c a x -> ProcM e b c x) -> A.Pat c a -> Either (ProcErr e b) x
bottomUpPM g f (A.Pat (JotP c0 p0)) = runPM (go p0) g c0
 where
  go p = bitraverse pure (\(JotP c1 p1) -> pushPM c1 (go p1)) p >>= f
