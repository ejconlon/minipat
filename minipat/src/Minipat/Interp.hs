-- | Interpreting patterns as streams
module Minipat.Interp
  ( Sel
  , SelFn
  , yesSelFn
  , noSelFn
  , InterpErr (..)
  , interpPat
  )
where

import Bowtie (Anno (..))
import Control.Applicative (Alternative (..))
import Control.Exception (Exception)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Data.Foldable (foldMap')
import Data.Foldable1 (foldl1')
import Data.Ratio ((%))
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty qualified as NESeq
import Minipat.Ast
import Minipat.Rand qualified as D
import Minipat.Rewrite (RwErr, RwT, rewriteM, throwRw)
import Minipat.Stream
  ( Stream (..)
  , streamConcat
  , streamDegradeBy
  , streamFast
  , streamFastBy
  , streamReplicate
  , streamSlow
  )
import Minipat.Time (Cycle (..), CycleDelta (..), spanActive, spanSplit)

-- | A sequence of selections applied to the given value
type Sel = Anno (Seq Select)

-- | A function combining selections with the given value.
-- Returning 'Nothing' indicates an error.
type SelFn a c = Seq Select -> a -> Maybe c

-- | Accept all selections.
yesSelFn :: SelFn a (Sel a)
yesSelFn ss = Just . Anno ss

-- | Forbit selections altogether.
noSelFn :: SelFn a a
noSelFn = \case
  Empty -> Just
  _ -> const Nothing

-- | An error interpreting a 'Pat' as a 'Stream'
data InterpErr
  = -- | When extent shorthands have not been previously eliminated
    InterpErrShort
  | -- | When selections are invalid
    InterpErrSel
  deriving stock (Eq, Ord, Show)

instance Exception InterpErr

type M b = ReaderT (Seq Select) (Except (RwErr InterpErr b))

runM :: M b a -> Either (RwErr InterpErr b) a
runM = runExcept . flip runReaderT Empty

lookInterp
  :: SelFn Factor Factor
  -> SelFn a c
  -> PatX b a (M b (Stream c, CycleDelta))
  -> RwT b (M b) (Stream c, CycleDelta)
lookInterp g h = \case
  PatPure a -> do
    ss <- lift ask
    maybe (throwRw InterpErrSel) (\c -> pure (pure c, 1)) (h ss a)
  PatSilence -> pure (empty, 1)
  PatExtent t ->
    case t of
      ExtentShort _ -> throwRw InterpErrShort
      ExtentLong melw u -> do
        (el, w) <- lift melw
        case u of
          LongExtentElongate f -> pure (el, CycleDelta (factorValue f * unCycleDelta w))
          LongExtentReplicate mf ->
            let v = maybe 2 fromInteger mf
            in  pure (streamReplicate v el 1, fromIntegral v)
  PatGroup (Group _ ty els) -> do
    els' <- lift (sequenceA els)
    case ty of
      GroupTypeSeq _ -> pure (streamConcat els', 1)
      GroupTypePar -> pure (foldl1' (<|>) (fmap fst els'), 1)
      GroupTypeRand ->
        let l = NESeq.length els
            f arc' =
              let s = D.arcSeed arc'
                  i = D.randInt l s
                  (el, w) = NESeq.index els' i
              in  unStream (streamFastBy (unCycleDelta w) el) arc'
        in  pure (Stream (foldMap' (f . spanActive . snd) . spanSplit), 1)
      GroupTypeAlt ->
        let l = NESeq.length els
            f z arc' =
              let i = mod (fromInteger (unCycle z)) l
                  (el, w) = NESeq.index els' i
              in  unStream (streamFastBy (unCycleDelta w) el) arc'
        in  pure (Stream (foldMap' (\(z, sp) -> f z (spanActive sp)) . spanSplit), 1)
  PatMod (Mod mx md) -> do
    case md of
      ModTypeSpeed (Speed dir spat) -> do
        spat' <- lift (subInterp g g spat)
        let f = case dir of
              SpeedDirFast -> streamFast
              SpeedDirSlow -> streamSlow
            spat'' = fmap factorValue spat'
        (r', w) <- lift mx
        pure (f spat'' r', w)
      ModTypeSelect s -> lift (local (:|> s) mx)
      ModTypeDegrade (Degrade dd) -> do
        let d = maybe (1 % 2) factorValue dd
        (r', w) <- lift mx
        let r'' = streamDegradeBy d r'
        pure (r'', w)
      ModTypeEuclid _ -> error "TODO"
  PatPoly (Poly _ _) -> error "TODO"

subInterp :: SelFn Factor Factor -> SelFn a c -> Pat b a -> M b (Stream c)
subInterp g h = fmap fst . rewriteM (lookInterp g h) . unPat

-- | Interpret the given 'Pat' as a 'Stream'
interpPat :: SelFn Factor Factor -> SelFn a c -> Pat b a -> Either (RwErr InterpErr b) (Stream c)
interpPat g h = runM . subInterp g h
