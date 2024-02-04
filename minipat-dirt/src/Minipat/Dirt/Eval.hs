{-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Eval
  ( liveEvalPat
  , liveEvalSoundPat
  )
where

import Dahdit.Midi.Osc (Datum (..), DatumType (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import Looksee (intP, sciP)
import Minipat.Ast (Ident (..), Pattern (..), Select (..))
import Minipat.Dirt.Osc (Attrs)
import Minipat.Eval (EvalEnv (..), evalPat)
import Minipat.Interp (InterpEnv (..), InterpErr (..), forbidInterpEnv)
import Minipat.Parser (P, identP)

datumP :: DatumType -> P Datum
datumP = \case
  DatumTypeInt32 -> fmap (DatumInt32 . fromInteger) intP
  DatumTypeInt64 -> fmap (DatumInt64 . fromInteger) intP
  DatumTypeFloat -> fmap (DatumFloat . realToFrac) sciP
  DatumTypeDouble -> fmap (DatumDouble . realToFrac) sciP
  DatumTypeString -> fmap (DatumString . unIdent) identP
  dt -> fail ("Datum type is not parseable: " <> show dt)

forbidEvalEnv :: DatumType -> EvalEnv e Datum
forbidEvalEnv dt = EvalEnv forbidInterpEnv (datumP dt)

liveEvalPat :: (Pattern f) => DatumType -> Text -> f Datum
liveEvalPat dt txt = either (pure patEmpty) id (evalPat @_ @Void (forbidEvalEnv dt) txt)

data SoundSelectErr = SoundSelectErr
  deriving stock (Eq, Ord, Show)

soundSelFn :: (Pattern f) => Select -> f Attrs -> Either (InterpErr SoundSelectErr) (f Attrs)
soundSelFn sel attrs =
  case sel of
    SelectSample n -> Right (fmap (Map.insert "note" (DatumInt32 (fromIntegral n))) attrs)
    _ -> Left (InterpErrEmbed SoundSelectErr)

soundProjFn :: Ident -> Attrs
soundProjFn = Map.singleton "sound" . DatumString . unIdent

soundInterpEnv :: InterpEnv SoundSelectErr Ident Attrs
soundInterpEnv = InterpEnv soundSelFn soundProjFn

soundEvalEnv :: EvalEnv SoundSelectErr Attrs
soundEvalEnv = EvalEnv soundInterpEnv identP

liveEvalSoundPat :: (Pattern f) => Text -> f Attrs
liveEvalSoundPat txt = either (pure patEmpty) id (evalPat soundEvalEnv txt)
