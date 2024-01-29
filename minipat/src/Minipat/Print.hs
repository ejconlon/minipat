-- | General utils for pretty-printing
module Minipat.Print
  ( render
  , Brace (..)
  , braceOpenChar
  , braceCloseChar
  , Sep (..)
  , sepChar
  )
where

import Data.Text (Text)
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as P
import Prettyprinter.Render.Text qualified as PRT

render :: (Pretty a) => a -> Text
render = PRT.renderStrict . P.layoutCompact . pretty

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
