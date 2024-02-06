{-# LANGUAGE OverloadedStrings #-}

-- | General utils for pretty-printing
module Minipat.Print
  ( prettyShow
  , prettyPrint
  , prettyTup
  , prettyRat
  , pretty
  , Brace (..)
  , braceOpenChar
  , braceCloseChar
  , Sep (..)
  , sepChar
  )
where

import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Prettyprinter (Doc, Pretty (..))
import Prettyprinter qualified as P
import Prettyprinter.Render.Text qualified as PRT

prettyShow :: (Pretty a) => a -> Text
prettyShow = PRT.renderStrict . P.layoutCompact . pretty

prettyPrint :: (Pretty a) => a -> IO ()
prettyPrint = TIO.putStrLn . prettyShow

prettyTup :: (Pretty a) => a -> a -> Doc ann
prettyTup el1 el2 = "(" <> pretty el1 <> ", " <> pretty el2 <> ")"

prettyRat :: Rational -> Doc ann
prettyRat r = P.hcat [pretty (numerator r), "/", pretty (denominator r)]

data Brace = BraceSquare | BraceCurly | BraceAngle | BraceParen
  deriving stock (Eq, Ord, Show)

braceOpenChar :: Brace -> Char
braceOpenChar = \case
  BraceSquare -> '['
  BraceCurly -> '{'
  BraceAngle -> '<'
  BraceParen -> '('

braceCloseChar :: Brace -> Char
braceCloseChar = \case
  BraceSquare -> ']'
  BraceCurly -> '}'
  BraceAngle -> '>'
  BraceParen -> ')'

data Sep = SepDot | SepComma | SepPipe
  deriving stock (Eq, Ord, Show, Enum, Bounded)

sepChar :: Sep -> Char
sepChar = \case
  SepDot -> '.'
  SepComma -> ','
  SepPipe -> '|'
