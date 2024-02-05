{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Textual parsing of patterns in mini-notation
module Minipat.Parser
  ( Loc (..)
  , P
  , ParseErr (..)
  , PPat
  , topPatP
  , identPatP
  , selectIdentPatP
  , factorP
  , identP
  , selectP
  )
where

import Bowtie (Anno (..), Jot, pattern JotP)
import Control.Exception (Exception)
import Control.Monad (guard, when)
import Control.Monad.Fix (fix)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Ratio (denominator, numerator)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Looksee qualified as L
import Minipat.Ast
import Minipat.Print (Brace (..), braceCloseChar, braceOpenChar)

-- Should be in Bowtie
annoJot :: Anno b (g a (Jot g b a)) -> Jot g b a
annoJot (Anno b x) = JotP b x

-- * The basics

-- | Error when parsing
data ParseErr
  = -- | Dot notation forbidden in the current position
    ParseErrDotForbidden
  | -- | Elongation not allowed in the current position
    ParseErrElongate
  | -- | Invalid quick ratio char
    ParseErrRatioChar !Char
  deriving stock (Eq, Ord, Show)

instance L.HasErrMessage ParseErr where
  getErrMessage = \case
    ParseErrDotForbidden -> ["Dot is not allowed in this position"]
    ParseErrElongate -> ["Elongate is not allowed in this position"]
    ParseErrRatioChar c -> ["Invalid ratio " <> T.singleton c]

instance Exception ParseErr

type P = L.Parser ParseErr

-- | The location in the source text
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

stripIdentP :: P Ident
stripIdentP = L.stripEndP identP

guardP :: (Char -> Bool) -> P ()
guardP f = do
  h <- L.lookP L.headP
  guard (f h)

identP :: P Ident
identP = fmap Ident (L.takeWhile1P isIdentChar)

selectP :: P a -> P s -> P (Select s a)
selectP pa ps = do
  a <- pa
  fmap (Select a) (L.optP (tokP ':' *> ps))

fracFactorP :: P Factor
fracFactorP = do
  stripTokP '('
  num <- L.stripEndP L.decP
  stripTokP '/'
  denom <- L.stripEndP L.udecP
  tokP ')'
  pure (FactorRational RationalPresFrac (num / denom))

numFactorP :: P Factor
numFactorP = do
  d <- L.decP
  pure $ case denominator d of
    1 -> FactorInteger (numerator d)
    _ -> FactorRational RationalPresDec d

quickFactorP :: P Factor
quickFactorP = do
  c <- L.headP
  case quickRatioUnRep c of
    Nothing -> L.throwP (ParseErrRatioChar c)
    Just qr -> pure (FactorQuickRatio qr)

factorP :: P Factor
factorP = do
  c <- L.lookP L.headP
  if isAlpha c
    then quickFactorP
    else case c of
      '(' -> fracFactorP
      _ -> numFactorP

bracedP :: Brace -> P a -> P a
bracedP b = L.betweenP (stripTokP (braceOpenChar b)) (tokP (braceCloseChar b))

speedFastP :: P s -> P (Speed s)
speedFastP ps = do
  tokP '*'
  Speed SpeedDirFast <$> ps

speedSlowP :: P s -> P (Speed s)
speedSlowP ps = do
  tokP '/'
  Speed SpeedDirSlow <$> ps

elongateShortP :: P Short
elongateShortP = ShortElongate <$ tokP '_'

replicateShortP :: P Short
replicateShortP = ShortReplicate <$ tokP '!'

elongateLongP :: P Elongate
elongateLongP = do
  tokP '@'
  Elongate <$> factorP

replicateLongP :: P Replicate
replicateLongP = do
  tokP '!'
  Replicate <$> L.optP L.uintP

degradeP :: P Degrade
degradeP = do
  tokP '?'
  fmap Degrade (L.optP factorP)

euclidP :: P Euclid
euclidP = do
  stripTokP '('
  x <- L.stripEndP L.uintP
  stripTokP ','
  y <- L.stripEndP L.uintP
  mu <- L.optP (stripTokP ',')
  euc <- case mu of
    Nothing -> pure (Euclid x y Nothing)
    Just _ -> fmap (Euclid x y . Just) (L.stripEndP L.uintP)
  tokP ')'
  pure euc

-- * Patterns

type PPat = Pat Loc

type UnPPat = UnPat Loc

silencePatP :: P (PPat a)
silencePatP = Pat <$> jotP (PatSilence <$ tokP '~')

shortElongatePatP :: P (PPat a)
shortElongatePatP = Pat <$> jotP (PatShort <$> elongateShortP)

shortReplicatePatP :: P (PPat a)
shortReplicatePatP = Pat <$> jotP (PatShort <$> replicateShortP)

withPatDecosP :: P (PPat Factor) -> PPat a -> P (PPat a)
withPatDecosP ps = go
 where
  go p@(Pat pp) = do
    mp' <- fmap (fmap Pat) . mayJotP $ do
      mc <- L.lookP L.unconsP
      case mc of
        Just '@' -> fmap (Just . PatMod . Mod pp . ModTypeElongate) elongateLongP
        Just '!' -> fmap (Just . PatMod . Mod pp . ModTypeReplicate) replicateLongP
        Just '*' -> fmap (Just . PatMod . Mod pp . ModTypeSpeed) (speedFastP ps)
        Just '/' -> fmap (Just . PatMod . Mod pp . ModTypeSpeed) (speedSlowP ps)
        Just '(' -> fmap (Just . PatMod . Mod pp . ModTypeEuclid) euclidP
        Just '?' -> fmap (Just . PatMod . Mod pp . ModTypeDegrade) degradeP
        _ -> pure Nothing
    case mp' of
      Just p' -> go p'
      Nothing -> pure p

spaceSeqPatP :: P (PPat a) -> P (Seq (UnPPat a))
spaceSeqPatP pr = go Empty
 where
  go !acc = do
    mcd <- L.lookP (liftA2 (,) L.unconsP L.unconsP)
    case mcd of
      (Just c, _) | not (isEndChar c) -> do
        Pat r <- L.stripEndP pr
        go (acc :|> r)
      _ -> pure acc

spaceGroupP :: P (PPat a) -> P (Anno Loc (Group (UnPPat a)))
spaceGroupP = annoP . fmap (Group 0 (GroupTypeSeq SeqPresSpace)) . spaceSeqPatP

unNestSeqPatP :: Anno Loc (Group (UnPPat a)) -> PPat a
unNestSeqPatP (Anno x p@(Group lvl _ acc)) =
  Pat $ case (acc, lvl) of
    (r :<| Empty, 0) -> r
    _ -> JotP x (PatGroup p)

nestedSeqPatP :: P (PPat a) -> P (PPat a)
nestedSeqPatP = fmap unNestSeqPatP . spaceGroupP

-- | Parses a square-braced pattern.
squarePatP :: P (PPat a) -> P (PPat a)
squarePatP pr =
  bracedP
    BraceSquare
    ( Pat . annoJot . fmap (\g -> PatGroup (g {groupLevel = groupLevel g + 1}))
        <$> groupPatP
          (Just ']')
          [ (',', GroupTypePar)
          , ('|', GroupTypeRand)
          , ('.', GroupTypeSeq SeqPresDot)
          ]
          pr
          (spaceGroupP pr)
    )

groupPatP
  :: Maybe Char
  -> [(Char, GroupType)]
  -> P (PPat a)
  -> P (Anno Loc (Group (UnPPat a)))
  -> P (Anno Loc (Group (UnPPat a)))
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
            let Pat p = unNestSeqPatP g
            stripTokP subDelim
            annoP (goRest subTy subDelim (Seq.singleton p))
          Nothing -> pure g
  goRest subTy subDelim !totalAcc = do
    Pat acc <- nestedSeqPatP pr
    let totalAcc' = totalAcc :|> acc
    mc <- L.lookP L.unconsP
    if mc == delim
      then pure (Group 0 subTy totalAcc')
      else do
        when (mc == Just subDelim) (stripTokP subDelim)
        goRest subTy subDelim totalAcc'

anglePatP :: P (PPat a) -> P (PPat a)
anglePatP = bracedP BraceAngle . fmap Pat . jotP . fmap (PatGroup . Group 0 GroupTypeAlt) . spaceSeqPatP

curlyPatP :: P (PPat a) -> P (PPat a)
curlyPatP pr = fmap Pat $ jotP $ do
  ps <- bracedP BraceCurly (L.sepBy1P (stripTokP ',') (fmap unPat (nestedSeqPatP pr)))
  mx <- L.lookP L.unconsP
  mc <-
    if mx == Just '%'
      then tokP '%' >> fmap Just L.uintP
      else pure Nothing
  pure (PatPoly (Poly ps mc))

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
    _ -> fmap Pat (jotP (fmap PatPure pa))

-- | Parses a recursive pattern (atoms and explicit braces).
rePatP :: P a -> P (PPat Factor) -> P (PPat a) -> P (PPat a)
rePatP pa pf pr = singlePatP pa pr >>= withPatDecosP pf

-- | Parses `x y . z w` sequence groups. Can consume the entire input.
outerGroupP :: P (PPat a) -> P (Anno Loc (Group (UnPPat a)))
outerGroupP pr =
  groupPatP
    Nothing
    [('.', GroupTypeSeq SeqPresDot)]
    pr
    (spaceGroupP pr)

-- | Parses top-level `x y . z w` patterns. Can consume the entire input.
outerPatP :: P (PPat a) -> P (PPat a)
outerPatP = fmap unNestSeqPatP . outerGroupP

-- | Parses a top-level pattern given parsers for atoms and signals.
patP :: P a -> P (PPat Factor) -> P (PPat a)
patP pa pf = L.spaceP >> outerPatP (fix (rePatP pa pf))

-- | Parses a top-level pattern of the given type.
topPatP :: P a -> P (PPat a)
topPatP p = patP p (fix (\pf -> rePatP factorP pf pf))

-- | Parses a top-level pattern of identifiers.
identPatP :: P (PPat Ident)
identPatP = topPatP identP

-- | Parses a top-level pattern of identifiers with selections.
selectIdentPatP :: P s -> P (PPat (Select s Ident))
selectIdentPatP = topPatP . selectP identP
