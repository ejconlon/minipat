-- | Pattern normalization (the step between parsing and interpreting)
module Minipat.Norm
  ( normPat
  )
where

import Bowtie (pattern JotP)
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty (NESeq (..))
import Data.Sequence.NonEmpty qualified as NESeq
import Minipat.Ast
  ( Extent (..)
  , Group (..)
  , GroupType (..)
  , LongExtent (..)
  , Pat (..)
  , PatF (..)
  , PatX
  , ShortExtent (..)
  , UnPat
  )
import Minipat.Rewrite (Rw, asksRw, overhaul, wrapRw)

foldNorm :: (b -> b -> b) -> Seq (UnPat b a) -> Seq (UnPat b a)
foldNorm f = goFirst
 where
  goFirst = \case
    Empty -> Empty
    (y :<| ys) -> goRest (NESeq.singleton y) ys
  goRest ws@(winit :||> wlast@(JotP wlb wlpf)) = \case
    Empty -> NESeq.toSeq ws
    y@(JotP b pf) :<| ys ->
      let ws' = case pf of
            PatExtent (ExtentShort s) ->
              case (s, wlpf) of
                (ShortExtentElongate, PatExtent (ExtentLong c (LongExtentElongate x))) ->
                  let pf' = PatExtent (ExtentLong c (LongExtentElongate (x + 1)))
                  in  winit :||> JotP (f wlb b) pf'
                (ShortExtentReplicate, PatExtent (ExtentLong c (LongExtentReplicate mx))) ->
                  let pf' = PatExtent (ExtentLong c (LongExtentReplicate (Just (maybe 3 (+ 1) mx))))
                  in  winit :||> JotP (f wlb b) pf'
                _ ->
                  let pf' = PatExtent $ ExtentLong wlast $ case s of
                        ShortExtentElongate -> LongExtentElongate 2
                        ShortExtentReplicate -> LongExtentReplicate Nothing
                  in  winit :||> JotP b pf'
            _ -> ws NESeq.|> y
      in  goRest ws' ys

subNorm :: (b -> b -> b) -> PatX b a (UnPat b a) -> Rw b (UnPat b a)
subNorm f x = case x of
  PatGroup (Group lvl ty ss) -> do
    -- Fold over sequences, eliminating time shorthands
    let ss' = case ty of
          GroupTypeSeq _ -> foldNorm f ss
          _ -> ss
    -- Unwrap any empty groups or singletons we find
    case ss' of
      Empty -> asksRw (`JotP` PatSilence)
      q :<| Empty -> pure q
      _ -> wrapRw (PatGroup (Group lvl ty ss'))
  _ -> wrapRw x

-- Someday we might want to expose this variant, which supports combining annotations
normPat' :: (b -> b -> b) -> Pat b a -> Pat b a
normPat' f = Pat . overhaul (subNorm f) . unPat

-- | Normalize the given pattern
normPat :: Pat b a -> Pat b a
normPat = normPat' (\_ b -> b)
