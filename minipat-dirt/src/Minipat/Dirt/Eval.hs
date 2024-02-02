{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Eval
  ( liveEvalPat
  , liveEvalSoundPat
  )
where

import Dahdit.Midi.Osc (Datum (..), DatumType (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Looksee (intP, sciP)
import Minipat.Ast (Ident (..), Select (..))
import Minipat.Dirt.Osc (Attrs)
import Minipat.Eval (evalPat, evalPatNoSel)
import Minipat.Interp (InterpErr (..))
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
liveEvalPat dt t = either (pure mempty) id (evalPatNoSel (datumP dt) t)

data SoundSelectErr = SoundSelectErr
  deriving stock (Eq, Ord, Show)

soundSelFn :: Select -> Stream Attrs -> Either (InterpErr SoundSelectErr) (Stream Attrs)
soundSelFn sel attrs =
  case sel of
    SelectSample n -> Right (fmap (Map.insert "note" (DatumInt32 (fromIntegral n))) attrs)
    _ -> Left (InterpErrEmbed SoundSelectErr)

soundProjFn :: Ident -> Attrs
soundProjFn = Map.singleton "sound" . DatumString . unIdent

liveEvalSoundPat :: Text -> Stream Attrs
liveEvalSoundPat t = either (pure mempty) id (evalPat soundSelFn soundProjFn identP t)
