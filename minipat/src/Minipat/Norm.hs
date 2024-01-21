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

foldNorm :: NESeq (A.UnPat b a) -> NESeq (A.UnPat b a)
foldNorm = goFirst
 where
  goFirst (y :<|| ys) = do
    goRest (NESeq.singleton y) ys
  goRest ws@(winit :||> wlast@(JotP wlb wlpf)) = \case
    Empty -> ws
    y@(JotP b pf) :<| ys ->
      let ws' = case pf of
            A.PatTime (A.TimeShort s) ->
              case (s, wlpf) of
                (A.ShortTimeReplicate, A.PatTime (A.TimeLong c (A.LongTimeReplicate x))) ->
                  error "TODO"
                (A.ShortTimeElongate, A.PatTime (A.TimeLong c (A.LongTimeElongate mx))) ->
                  error "TODO"
                _ ->
                  let pf' = A.PatTime $ A.TimeLong wlast $ case s of
                        A.ShortTimeElongate -> A.LongTimeElongate 2
                        A.ShortTimeReplicate -> A.LongTimeReplicate Nothing
                  in winit :||> JotP b pf'
            A.PatTime (A.TimeLong q l) ->
              case (l, wlpf) of
                (A.LongTimeReplicate i, A.PatTime (A.TimeLong c (A.LongTimeReplicate x))) ->
                  error "TODO"
                (A.LongTimeElongate i, A.PatTime (A.TimeLong c (A.LongTimeElongate mx))) ->
                  error "TODO"
                _ -> ws
            _ -> ws NESeq.|> y
      in  goRest ws' ys

subNorm :: A.PatX b a (A.UnPat b a) -> R.Rw b (A.UnPat b a)
subNorm x = case x of
  A.PatGroup (A.Group lvl ty ss) -> do
    -- Fold over sequences, eliminating time shorthands
    let ss' = case ty of
          A.GroupTypeSeq _ -> foldNorm ss
          _ -> ss
    -- Unwrap any group singletons we find
    case ss' of
      q :<|| Empty -> pure q
      _ -> R.wrapRw (A.PatGroup (A.Group lvl ty ss'))
  _ -> R.wrapRw x

normPat :: A.Pat b a -> A.Pat b a
normPat = A.Pat . R.overhaul subNorm . A.unPat
