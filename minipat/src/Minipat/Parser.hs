{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Minipat.Parser
  ( Loc (..)
  , P
  , ParseErr (..)
  , PPat
  , identPatP
  , factorP
  , identP
  )
where

import Bowtie (Anno (..), Jot, pattern JotP)
import Control.Exception (Exception)
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Ratio (denominator, numerator)
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty (NESeq (..), (|>))
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Looksee qualified as L
import Minipat.Ast qualified as A
import Minipat.Print (Brace (..), braceCloseChar, braceOpenChar)

-- Should be in Bowtie
annoJot :: Anno b (g a (Jot g b a)) -> Jot g b a
annoJot (Anno b x) = JotP b x

-- * The basics

data ParseErr
  = ParseErrDotForbidden
  | ParseErrEmpty
  | ParseErrElongate
  | ParseErrRatioChar !Char
  deriving stock (Eq, Ord, Show)

instance L.HasErrMessage ParseErr where
  getErrMessage = \case
    ParseErrDotForbidden -> ["Dot is not allowed in this position"]
    ParseErrEmpty -> ["Sequence is empty"]
    ParseErrElongate -> ["Elongate is not allowed in this position"]
    ParseErrRatioChar c -> ["Invalid ratio " <> T.singleton c]

instance Exception ParseErr

type P = L.Parser ParseErr

newtype Loc = Loc {unLoc :: L.Span Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Semigroup Loc where
  Loc (L.Span a1 b1) <> Loc (L.Span a2 b2) = Loc (L.Span (min a1 a2) (max b1 b2))

annoP :: P a -> P (Anno Loc a)
annoP pa = do
  L.Span x1 _ <- L.spanP
  -- Lookahead to get result and end
  (a, L.Span x2 _) <- L.lookP ((,) <$> pa <*> L.spanP)
  -- Now take consumed text
  consumed <- L.takeExactP (x2 - x1)
  -- and subtract leading/trailing spaces from span
  let startSpaces = T.takeWhile isSpace consumed
      endSpaces = T.takeWhileEnd isSpace consumed
      x1' = x1 + T.length startSpaces
      x2' = x2 - T.length endSpaces
  pure (Anno (Loc (L.Span x1' x2')) a)

mayAnnoP :: P (Maybe a) -> P (Maybe (Anno Loc a))
mayAnnoP pa = do
  flip fmap (annoP pa) $ \(Anno b ma) ->
    case ma of
      Nothing -> Nothing
      Just a -> Just (Anno b a)

jotP :: P (g a (Jot g Loc a)) -> P (Jot g Loc a)
jotP = fmap annoJot . annoP

mayJotP :: P (Maybe (g a (Jot g Loc a))) -> P (Maybe (Jot g Loc a))
mayJotP = fmap (fmap annoJot) . mayAnnoP

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '-'

isBraceHd :: Char -> Bool
isBraceHd c = c == '[' || c == '<' || c == '{' || c == '('

isEndChar :: Char -> Bool
isEndChar c = c == ')' || c == ']' || c == '}' || c == '>' || c == '|' || c == '.' || c == ',' || isSpace c

stripTokP :: Char -> P ()
stripTokP = L.stripEndP . tokP

tokP :: Char -> P ()
tokP = L.charP_

stripIdentP :: P A.Ident
stripIdentP = L.stripEndP identP

identP :: P A.Ident
identP = fmap A.Ident (L.takeWhile1P isIdentChar)

fracFactorP :: P A.Factor
fracFactorP = do
  stripTokP '('
  num <- L.stripEndP L.decP
  stripTokP '/'
  denom <- L.stripEndP L.udecP
  tokP ')'
  pure (A.FactorRational A.RationalPresFrac (num / denom))

numFactorP :: P A.Factor
numFactorP = do
  d <- L.decP
  pure $ case denominator d of
    1 -> A.FactorInteger (numerator d)
    _ -> A.FactorRational A.RationalPresDec d

quickFactorP :: P A.Factor
quickFactorP = do
  c <- L.headP
  case A.quickRatioUnRep c of
    Nothing -> L.throwP (ParseErrRatioChar c)
    Just qr -> pure (A.FactorQuickRatio qr)

factorP :: P A.Factor
factorP = do
  c <- L.lookP L.headP
  if isAlpha c
    then quickFactorP
    else case c of
      '(' -> fracFactorP
      _ -> numFactorP

bracedP :: Brace -> P a -> P a
bracedP b = L.betweenP (stripTokP (braceOpenChar b)) (tokP (braceCloseChar b))

selectP :: P A.Select
selectP = do
  tokP ':'
  isNum <- L.lookP (fmap isDigit L.headP)
  if isNum
    then fmap A.SelectSample L.uintP
    else fmap A.SelectTransform identP

speedFastP :: P s -> P (A.Speed s)
speedFastP ps = do
  tokP '*'
  A.Speed A.SpeedDirFast <$> ps

speedSlowP :: P s -> P (A.Speed s)
speedSlowP ps = do
  tokP '/'
  A.Speed A.SpeedDirSlow <$> ps

elongateShortP :: P A.ShortTime
elongateShortP = A.ShortTimeElongate <$ tokP '_'

replicateShortP :: P A.ShortTime
replicateShortP = A.ShortTimeReplicate <$ tokP '!'

elongateLongP :: P A.LongTime
elongateLongP = do
  tokP '@'
  A.LongTimeElongate <$> factorP

replicateLongP :: P A.LongTime
replicateLongP = do
  tokP '!'
  A.LongTimeReplicate <$> L.optP L.uintP

degradeP :: P A.Degrade
degradeP = do
  tokP '?'
  fmap A.Degrade (L.optP factorP)

euclidP :: P A.Euclid
euclidP = do
  stripTokP '('
  x <- L.stripEndP L.uintP
  stripTokP ','
  y <- L.stripEndP L.uintP
  mu <- L.optP (stripTokP ',')
  euc <- case mu of
    Nothing -> pure (A.Euclid x y Nothing)
    Just _ -> fmap (A.Euclid x y . Just) (L.stripEndP L.uintP)
  tokP ')'
  pure euc

-- * Patterns

type PPat = A.Pat Loc

type UnPPat = A.UnPat Loc

silencePatP :: P (PPat a)
silencePatP = A.Pat <$> jotP (A.PatSilence <$ tokP '~')

shortElongatePatP :: P (PPat a)
shortElongatePatP = A.Pat <$> jotP (A.PatTime . A.TimeShort <$> elongateShortP)

shortReplicatePatP :: P (PPat a)
shortReplicatePatP = A.Pat <$> jotP (A.PatTime . A.TimeShort <$> replicateShortP)

withPatDecosP :: P (PPat A.Factor) -> PPat a -> P (PPat a)
withPatDecosP ps = go
 where
  go p@(A.Pat pp) = do
    mp' <- fmap (fmap A.Pat) . mayJotP $ do
      mc <- L.lookP L.unconsP
      case mc of
        Just '@' -> fmap (Just . A.PatTime . A.TimeLong pp) elongateLongP
        Just '!' -> fmap (Just . A.PatTime . A.TimeLong pp) replicateLongP
        Just ':' -> fmap (Just . A.PatMod . A.Mod pp . A.ModTypeSelect) selectP
        Just '*' -> fmap (Just . A.PatMod . A.Mod pp . A.ModTypeSpeed) (speedFastP ps)
        Just '/' -> fmap (Just . A.PatMod . A.Mod pp . A.ModTypeSpeed) (speedSlowP ps)
        Just '(' -> fmap (Just . A.PatMod . A.Mod pp . A.ModTypeEuclid) euclidP
        Just '?' -> fmap (Just . A.PatMod . A.Mod pp . A.ModTypeDegrade) degradeP
        _ -> pure Nothing
    case mp' of
      Just p' -> go p'
      Nothing -> pure p

spaceSeqPatP :: P (PPat a) -> P (NESeq (UnPPat a))
spaceSeqPatP pr = go Empty
 where
  go !acc = do
    mcd <- L.lookP (liftA2 (,) L.unconsP L.unconsP)
    case mcd of
      (Just c, _) | not (isEndChar c) -> do
        A.Pat r <- L.stripEndP pr
        go (acc :|> r)
      _ -> case NESeq.nonEmptySeq acc of
        Nothing -> L.throwP ParseErrEmpty
        Just neAcc -> pure neAcc

spaceGroupP :: P (PPat a) -> P (Anno Loc (A.Group (UnPPat a)))
spaceGroupP = annoP . fmap (A.Group 0 (A.GroupTypeSeq A.SeqPresSpace)) . spaceSeqPatP

unNestSeqPatP :: Anno Loc (A.Group (UnPPat a)) -> PPat a
unNestSeqPatP (Anno x p@(A.Group lvl _ acc)) =
  A.Pat $ case (acc, lvl) of
    (r :<|| Empty, 0) -> r
    _ -> JotP x (A.PatGroup p)

nestedSeqPatP :: P (PPat a) -> P (PPat a)
nestedSeqPatP = fmap unNestSeqPatP . spaceGroupP

-- | Parses a square-braced pattern.
squarePatP :: P (PPat a) -> P (PPat a)
squarePatP pr =
  bracedP
    BraceSquare
    ( A.Pat . annoJot . fmap (\g -> A.PatGroup (g {A.gpLevel = A.gpLevel g + 1}))
        <$> groupPatP
          (Just ']')
          [ (',', A.GroupTypePar)
          , ('|', A.GroupTypeRand)
          , ('.', A.GroupTypeSeq A.SeqPresDot)
          ]
          pr
          (spaceGroupP pr)
    )

groupPatP
  :: Maybe Char
  -> [(Char, A.GroupType)]
  -> P (PPat a)
  -> P (Anno Loc (A.Group (UnPPat a)))
  -> P (Anno Loc (A.Group (UnPPat a)))
groupPatP delim opts pr pg = goStart
 where
  goStart = do
    g <- pg
    mc <- L.lookP L.unconsP
    if mc == delim
      then pure g
      else do
        case mc >>= \c -> fmap (c,) (lookup c opts) of
          Just (subDelim, subTy) -> do
            let A.Pat p = unNestSeqPatP g
            stripTokP subDelim
            annoP (goRest subTy subDelim (NESeq.singleton p))
          Nothing -> pure g
  goRest subTy subDelim !totalAcc = do
    A.Pat acc <- nestedSeqPatP pr
    let totalAcc' = totalAcc |> acc
    mc <- L.lookP L.unconsP
    if mc == delim
      then pure (A.Group 0 subTy totalAcc')
      else do
        when (mc == Just subDelim) (stripTokP subDelim)
        goRest subTy subDelim totalAcc'

anglePatP :: P (PPat a) -> P (PPat a)
anglePatP = bracedP BraceAngle . fmap A.Pat . jotP . fmap (A.PatGroup . A.Group 0 A.GroupTypeAlt) . spaceSeqPatP

curlyPatP :: P (PPat a) -> P (PPat a)
curlyPatP pr = fmap A.Pat $ jotP $ do
  ps <- bracedP BraceCurly (fmap NESeq.unsafeFromSeq (L.sepBy1P (stripTokP ',') (fmap A.unPat (nestedSeqPatP pr))))
  mx <- L.lookP L.unconsP
  mc <-
    if mx == Just '%'
      then tokP '%' >> fmap Just L.uintP
      else pure Nothing
  pure (A.PatPoly (A.PolyPat ps mc))

singlePatP :: P a -> P (PPat a) -> P (PPat a)
singlePatP pa pr = do
  mc <- L.lookP L.unconsP
  case mc of
    Just '[' -> squarePatP pr
    Just '<' -> anglePatP pr
    Just '{' -> curlyPatP pr
    Just '~' -> silencePatP
    Just '_' -> shortElongatePatP
    Just '!' -> shortReplicatePatP
    _ -> fmap A.Pat (jotP (fmap A.PatPure pa))

-- | Parses a recursive pattern (atoms and explicit braces).
rePatP :: P a -> P (PPat A.Factor) -> P (PPat a) -> P (PPat a)
rePatP pa pf pr = singlePatP pa pr >>= withPatDecosP pf

-- | Parses `x y . z w` sequence groups. Can consume the entire input.
outerGroupP :: P (PPat a) -> P (Anno Loc (A.Group (UnPPat a)))
outerGroupP pr =
  groupPatP
    Nothing
    [('.', A.GroupTypeSeq A.SeqPresDot)]
    pr
    (spaceGroupP pr)

-- | Parses top-level `x y . z w` patterns. Can consume the entire input.
outerPatP :: P (PPat a) -> P (PPat a)
outerPatP = fmap unNestSeqPatP . outerGroupP

-- | Parses a top-level pattern given parsers for atoms and signals.
patP :: P a -> P (PPat A.Factor) -> P (PPat a)
patP pa pf = L.spaceP >> outerPatP (fix (rePatP pa pf))

-- | Parses a top-level pattern with variables.
identPatP :: P (PPat A.Ident)
identPatP = patP identP (fix (\pf -> rePatP factorP pf pf))
