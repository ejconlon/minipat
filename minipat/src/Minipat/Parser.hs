{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Minipat.Parser where

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
import Looksee
  ( HasErrMessage (..)
  , Parser
  , Span (..)
  , betweenP
  , charP_
  , decP
  , headP
  , lookP
  , optP
  , sepBy1P
  , spaceP
  , spanP
  , stripEndP
  , takeExactP
  , takeWhile1P
  , throwP
  , udecP
  , uintP
  , unconsP
  )
import Minipat.Ast
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

instance HasErrMessage ParseErr where
  getErrMessage = \case
    ParseErrDotForbidden -> ["Dot is not allowed in this position"]
    ParseErrEmpty -> ["Sequence is empty"]
    ParseErrElongate -> ["Elongate is not allowed in this position"]
    ParseErrRatioChar c -> ["Invalid ratio " <> T.singleton c]

instance Exception ParseErr

type P = Parser ParseErr

newtype Loc = Loc {unLoc :: Span Int}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance Semigroup Loc where
  Loc (Span a1 b1) <> Loc (Span a2 b2) = Loc (Span (min a1 a2) (max b1 b2))

annoP :: P a -> P (Anno Loc a)
annoP pa = do
  Span x1 _ <- spanP
  -- Lookahead to get result and end
  (a, Span x2 _) <- lookP ((,) <$> pa <*> spanP)
  -- Now take consumed text
  consumed <- takeExactP (x2 - x1)
  -- and subtract leading/trailing spaces from span
  let startSpaces = T.takeWhile isSpace consumed
      endSpaces = T.takeWhileEnd isSpace consumed
      x1' = x1 + T.length startSpaces
      x2' = x2 - T.length endSpaces
  pure (Anno (Loc (Span x1' x2')) a)

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
stripTokP = stripEndP . tokP

tokP :: Char -> P ()
tokP = charP_

stripIdentP :: P Ident
stripIdentP = stripEndP identP

identP :: P Ident
identP = fmap Ident (takeWhile1P isIdentChar)

fracFactorP :: P Factor
fracFactorP = do
  stripTokP '('
  num <- stripEndP decP
  stripTokP '/'
  denom <- stripEndP udecP
  tokP ')'
  pure (FactorRational RationalPresFrac (num / denom))

numFactorP :: P Factor
numFactorP = do
  d <- decP
  pure $ case denominator d of
    1 -> FactorInteger (numerator d)
    _ -> FactorRational RationalPresDec d

quickFactorP :: P Factor
quickFactorP = do
  c <- headP
  case quickRatioUnRep c of
    Nothing -> throwP (ParseErrRatioChar c)
    Just qr -> pure (FactorQuickRatio qr)

factorP :: P Factor
factorP = do
  c <- lookP headP
  if isAlpha c
    then quickFactorP
    else case c of
      '(' -> fracFactorP
      _ -> numFactorP

bracedP :: Brace -> P a -> P a
bracedP b = betweenP (stripTokP (braceOpenChar b)) (tokP (braceCloseChar b))

selectP :: P Select
selectP = do
  tokP ':'
  isNum <- lookP (fmap isDigit headP)
  if isNum
    then fmap SelectSample uintP
    else fmap SelectTransform identP

speedFastP :: P s -> P (Speed s)
speedFastP ps = do
  tokP '*'
  Speed SpeedDirFast <$> ps

speedSlowP :: P s -> P (Speed s)
speedSlowP ps = do
  tokP '/'
  Speed SpeedDirSlow <$> ps

elongateShortP :: P ShortTime
elongateShortP = ShortTimeElongate <$ tokP '_'

replicateShortP :: P ShortTime
replicateShortP = ShortTimeReplicate <$ tokP '!'

elongateLongP :: P LongTime
elongateLongP = do
  tokP '@'
  LongTimeElongate <$> factorP

replicateLongP :: P LongTime
replicateLongP = do
  tokP '!'
  LongTimeReplicate <$> optP uintP

degradeP :: P Degrade
degradeP = do
  tokP '?'
  fmap Degrade (optP factorP)

euclidP :: P Euclid
euclidP = do
  stripTokP '('
  x <- stripEndP uintP
  stripTokP ','
  y <- stripEndP uintP
  mu <- optP (stripTokP ',')
  euc <- case mu of
    Nothing -> pure (Euclid x y Nothing)
    Just _ -> fmap (Euclid x y . Just) (stripEndP uintP)
  tokP ')'
  pure euc

-- * Patterns

type PPat = Pat Loc

type UnPPat = UnPat Loc

silencePatP :: P (PPat a)
silencePatP = Pat <$> jotP (PatSilence <$ tokP '~')

shortElongatePatP :: P (PPat a)
shortElongatePatP = Pat <$> jotP (PatTime . TimeShort <$> elongateShortP)

shortReplicatePatP :: P (PPat a)
shortReplicatePatP = Pat <$> jotP (PatTime . TimeShort <$> replicateShortP)

withPatDecosP :: P (PPat Factor) -> PPat a -> P (PPat a)
withPatDecosP ps = go
 where
  go p@(Pat pp) = do
    mp' <- fmap (fmap Pat) . mayJotP $ do
      mc <- lookP unconsP
      case mc of
        Just '@' -> fmap (Just . PatTime . TimeLong pp) elongateLongP
        Just '!' -> fmap (Just . PatTime . TimeLong pp) replicateLongP
        Just ':' -> fmap (Just . PatMod . Mod pp . ModPatSelect) selectP
        Just '*' -> fmap (Just . PatMod . Mod pp . ModPatSpeed) (speedFastP ps)
        Just '/' -> fmap (Just . PatMod . Mod pp . ModPatSpeed) (speedSlowP ps)
        Just '(' -> fmap (Just . PatMod . Mod pp . ModPatEuclid) euclidP
        Just '?' -> fmap (Just . PatMod . Mod pp . ModPatDegrade) degradeP
        _ -> pure Nothing
    case mp' of
      Just p' -> go p'
      Nothing -> pure p

spaceSeqPatP :: P (PPat a) -> P (NESeq (UnPPat a))
spaceSeqPatP pr = go Empty
 where
  go !acc = do
    mcd <- lookP (liftA2 (,) unconsP unconsP)
    case mcd of
      (Just c, _) | not (isEndChar c) -> do
        Pat r <- stripEndP pr
        go (acc :|> r)
      _ -> case NESeq.nonEmptySeq acc of
        Nothing -> throwP ParseErrEmpty
        Just neAcc -> pure neAcc

spaceGroupPatP :: P (PPat a) -> P (Anno Loc (GroupPat (UnPPat a)))
spaceGroupPatP = annoP . fmap (GroupPat 0 (GroupPatTypeSeq SeqPresSpace)) . spaceSeqPatP

unNestSeqPatP :: Anno Loc (GroupPat (UnPPat a)) -> PPat a
unNestSeqPatP (Anno x p@(GroupPat lvl _ acc)) =
  Pat $ case (acc, lvl) of
    (r :<|| Empty, 0) -> r
    _ -> JotP x (PatGroup p)

nestedSeqPatP :: P (PPat a) -> P (PPat a)
nestedSeqPatP = fmap unNestSeqPatP . spaceGroupPatP

-- | Parses a square-braced pattern.
squarePatP :: P (PPat a) -> P (PPat a)
squarePatP pr =
  bracedP
    BraceSquare
    ( Pat . annoJot . fmap (\g -> PatGroup (g {gpLevel = gpLevel g + 1}))
        <$> groupPatP
          (Just ']')
          [(',', GroupPatTypePar), ('|', GroupPatTypeRand), ('.', GroupPatTypeSeq SeqPresDot)]
          pr
          (spaceGroupPatP pr)
    )

groupPatP
  :: Maybe Char
  -> [(Char, GroupPatType)]
  -> P (PPat a)
  -> P (Anno Loc (GroupPat (UnPPat a)))
  -> P (Anno Loc (GroupPat (UnPPat a)))
groupPatP delim opts pr pg = goStart
 where
  goStart = do
    g <- pg
    mc <- lookP unconsP
    if mc == delim
      then pure g
      else do
        case mc >>= \c -> fmap (c,) (lookup c opts) of
          Just (subDelim, subTy) -> do
            let Pat p = unNestSeqPatP g
            stripTokP subDelim
            annoP (goRest subTy subDelim (NESeq.singleton p))
          Nothing -> pure g
  goRest subTy subDelim !totalAcc = do
    Pat acc <- nestedSeqPatP pr
    let totalAcc' = totalAcc |> acc
    mc <- lookP unconsP
    if mc == delim
      then pure (GroupPat 0 subTy totalAcc')
      else do
        when (mc == Just subDelim) (stripTokP subDelim)
        goRest subTy subDelim totalAcc'

anglePatP :: P (PPat a) -> P (PPat a)
anglePatP = bracedP BraceAngle . fmap Pat . jotP . fmap (PatGroup . GroupPat 0 GroupPatTypeAlt) . spaceSeqPatP

curlyPatP :: P (PPat a) -> P (PPat a)
curlyPatP pr = fmap Pat $ jotP $ do
  ps <- bracedP BraceCurly (fmap NESeq.unsafeFromSeq (sepBy1P (stripTokP ',') (fmap unPat (nestedSeqPatP pr))))
  mx <- lookP unconsP
  mc <-
    if mx == Just '%'
      then tokP '%' >> fmap Just uintP
      else pure Nothing
  pure (PatPoly (PolyPat ps mc))

singlePatP :: P a -> P (PPat a) -> P (PPat a)
singlePatP pa pr = do
  mc <- lookP unconsP
  case mc of
    Just '[' -> squarePatP pr
    Just '<' -> anglePatP pr
    Just '{' -> curlyPatP pr
    Just '~' -> silencePatP
    Just '_' -> shortElongatePatP
    Just '!' -> shortReplicatePatP
    _ -> fmap Pat (jotP (fmap PatPure pa))

-- | Parses a recursive pattern (atoms and explicit braces).
rePatP :: P a -> P (PPat Factor) -> P (PPat a) -> P (PPat a)
rePatP pa pf pr = singlePatP pa pr >>= withPatDecosP pf

-- | Parses `x y . z w` sequence groups. Can consume the entire input.
outerGroupPatP :: P (PPat a) -> P (Anno Loc (GroupPat (UnPPat a)))
outerGroupPatP pr =
  groupPatP
    Nothing
    [('.', GroupPatTypeSeq SeqPresDot)]
    pr
    (spaceGroupPatP pr)

-- | Parses top-level `x y . z w` patterns. Can consume the entire input.
outerPatP :: P (PPat a) -> P (PPat a)
outerPatP = fmap unNestSeqPatP . outerGroupPatP

-- | Parses a top-level pattern given parsers for atoms and signals.
patP :: P a -> P (PPat Factor) -> P (PPat a)
patP pa pf = spaceP >> outerPatP (fix (rePatP pa pf))

-- | Parses a top-level pattern with variables.
identPatP :: P (PPat Ident)
identPatP = patP identP (fix (\pf -> rePatP factorP pf pf))
