module Minipat.Norm
  ( normPat
  )
where

import Bowtie (pattern JotP)
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty (NESeq (..))
import Data.Sequence.NonEmpty qualified as NESeq
import Minipat.Ast qualified as A
import Minipat.Rewrite qualified as R

foldNorm :: (b -> b -> b) -> NESeq (A.UnPat b a) -> NESeq (A.UnPat b a)
foldNorm f = goFirst
 where
  goFirst (y :<|| ys) = do
    goRest (NESeq.singleton y) ys
  goRest ws@(winit :||> wlast@(JotP wlb wlpf)) = \case
    Empty -> ws
    y@(JotP b pf) :<| ys ->
      let ws' = case pf of
            A.PatTime (A.TimeShort s) ->
              case (s, wlpf) of
                (A.ShortTimeElongate, A.PatTime (A.TimeLong c (A.LongTimeElongate x))) ->
                  let pf' = A.PatTime (A.TimeLong c (A.LongTimeElongate (x + 1)))
                  in  winit :||> JotP (f wlb b) pf'
                (A.ShortTimeReplicate, A.PatTime (A.TimeLong c (A.LongTimeReplicate mx))) ->
                  let pf' = A.PatTime (A.TimeLong c (A.LongTimeReplicate (Just (maybe 3 (+ 1) mx))))
                  in  winit :||> JotP (f wlb b) pf'
                _ ->
                  let pf' = A.PatTime $ A.TimeLong wlast $ case s of
                        A.ShortTimeElongate -> A.LongTimeElongate 2
                        A.ShortTimeReplicate -> A.LongTimeReplicate Nothing
                  in  winit :||> JotP b pf'
            _ -> ws NESeq.|> y
      in  goRest ws' ys

subNorm :: (b -> b -> b) -> A.PatX b a (A.UnPat b a) -> R.Rw b (A.UnPat b a)
subNorm f x = case x of
  A.PatGroup (A.Group lvl ty ss) -> do
    -- Fold over sequences, eliminating time shorthands
    let ss' = case ty of
          A.GroupTypeSeq _ -> foldNorm f ss
          _ -> ss
    -- Unwrap any group singletons we find
    case ss' of
      q :<|| Empty -> pure q
      _ -> R.wrapRw (A.PatGroup (A.Group lvl ty ss'))
  _ -> R.wrapRw x

normPat' :: (b -> b -> b) -> A.Pat b a -> A.Pat b a
normPat' f = A.Pat . R.overhaul (subNorm f) . A.unPat

normPat :: A.Pat b a -> A.Pat b a
normPat = normPat' (\_ b -> b)
