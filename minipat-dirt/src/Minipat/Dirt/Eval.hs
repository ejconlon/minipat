{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Eval
  ( liveEvalPat
  , liveEvalSoundPat
  )
where

import Control.Exception (throw)
import Dahdit.Midi.Osc (Datum (..), DatumType (..))
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Looksee (intP, sciP)
import Minipat.Ast (Ident (..), Select (..))
import Minipat.Dirt.Osc (Attrs)
import Minipat.Eval (evalPat)
import Minipat.Interp (noSelFn)
import Minipat.Parser (P, identP)
import Minipat.Stream (Stream)

datumP :: DatumType -> P Datum
datumP = \case
  DatumTypeInt32 -> fmap (DatumInt32 . fromInteger) intP
  DatumTypeInt64 -> fmap (DatumInt64 . fromInteger) intP
  DatumTypeFloat -> fmap (DatumFloat . realToFrac) sciP
  DatumTypeDouble -> fmap (DatumDouble . realToFrac) sciP
  DatumTypeString -> fmap (DatumString . unIdent) identP
  dt -> fail ("Datum type is not parseable: " <> show dt)

liveEvalPat :: DatumType -> Text -> Stream Datum
liveEvalPat dt t = either throw id (evalPat noSelFn noSelFn (datumP dt) t)

liveEvalSoundPat :: Text -> Stream Attrs
liveEvalSoundPat t = either throw id (evalPat noSelFn selFn identP t)
 where
  selFn sels (Ident s) =
    case sels of
      Empty ->
        Just $
          Map.fromList
            [ ("sound", DatumString s)
            ]
      SelectSample n :<| Empty ->
        Just $
          Map.fromList
            [ ("sound", DatumString s)
            , ("note", DatumInt32 (fromIntegral n))
            ]
      _ -> Nothing
