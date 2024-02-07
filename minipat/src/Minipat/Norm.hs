-- | Pattern normalization (the step between parsing and interpreting)
module Minipat.Norm
  ( normPat
  )
where

import Bowtie (pattern JotP)
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty (NESeq (..))
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Void (Void)
import Minipat.Ast
  ( Elongate (..)
  , Group (..)
  , GroupType (..)
  , Mod (..)
  , ModType (..)
  , Pat (..)
  , PatF (..)
  , Replicate (..)
  , Short (..)
  , UnPat
  )
import Minipat.Rewrite (Rw, patNatRw, peeksRw, runPatRw, unwrapAnnoErr, wrapRw)

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
            PatShort s ->
              case (s, wlpf) of
                (ShortElongate, PatMod (Mod c (ModTypeElongate (Elongate x)))) ->
                  let pf' = PatMod (Mod c (ModTypeElongate (Elongate (x + 1))))
                  in  winit :||> JotP (f wlb b) pf'
                (ShortReplicate, PatMod (Mod c (ModTypeReplicate (Replicate mx)))) ->
                  let pf' = PatMod (Mod c (ModTypeReplicate (Replicate (Just (maybe 3 (+ 1) mx)))))
                  in  winit :||> JotP (f wlb b) pf'
                _ ->
                  let pf' = PatMod $ Mod wlast $ case s of
                        ShortElongate -> ModTypeElongate (Elongate 2)
                        ShortReplicate -> ModTypeReplicate (Replicate Nothing)
                  in  winit :||> JotP b pf'
            _ -> ws NESeq.|> y
      in  goRest ws' ys

subNorm :: (b -> b -> b) -> PatF b a (UnPat b a) -> Rw b Void (UnPat b a)
subNorm f x = case x of
  PatGroup (Group lvl ty ss) -> do
    -- Fold over sequences, eliminating time shorthands
    let ss' = case ty of
          GroupTypeSeq _ -> foldNorm f ss
          _ -> ss
    -- Unwrap any empty groups or singletons we find
    case ss' of
      Empty -> peeksRw (`JotP` PatSilence)
      q :<| Empty -> pure q
      _ -> wrapRw (PatGroup (Group lvl ty ss'))
  _ -> wrapRw x

-- Someday we might want to expose this variant, which supports
-- combining annotations any way we choose
normPat' :: (b -> b -> b) -> Pat b a -> Pat b a
normPat' f = unwrapAnnoErr . runPatRw (patNatRw (subNorm f))

-- | Normalize the given pattern
normPat :: (Semigroup b) => Pat b a -> Pat b a
normPat = normPat' (<>)
