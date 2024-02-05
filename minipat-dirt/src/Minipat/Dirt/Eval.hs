-- {-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Eval
  -- ( liveEvalPat
  -- , liveEvalSoundPat
  -- , liveEvalNotePat
  -- )
where

-- import Control.Exception (Exception)
-- import Dahdit.Midi.Osc (Datum (..), DatumType (..))
-- import Data.Map.Strict qualified as Map
-- import Data.Text (Text)
-- import Data.Void (Void)
-- import Looksee (intP, sciP)
-- import Minipat.Ast (Ident (..), Pattern (..), Select (..))
-- import Minipat.Dirt.Notes (LinNote)
-- import Minipat.Dirt.Osc (Attrs)
-- import Minipat.Eval (evalPat)
-- import Minipat.Interp (InterpErr (..))
-- import Minipat.Parser (P, identP)
--
-- datumP :: DatumType -> P Datum
-- datumP = \case
--   DatumTypeInt32 -> fmap (DatumInt32 . fromInteger) intP
--   DatumTypeInt64 -> fmap (DatumInt64 . fromInteger) intP
--   DatumTypeFloat -> fmap (DatumFloat . realToFrac) sciP
--   DatumTypeDouble -> fmap (DatumDouble . realToFrac) sciP
--   DatumTypeString -> fmap (DatumString . unIdent) identP
--   dt -> fail ("Datum type is not parseable: " <> show dt)
--
-- forbidEvalEnv :: DatumType -> EvalEnv e Datum
-- forbidEvalEnv dt = EvalEnv forbidInterpEnv (datumP dt)
--
-- liveEvalPat :: (Pattern f) => DatumType -> Text -> f Datum
-- liveEvalPat dt txt = either (pure patEmpty) id (evalPat @_ @Void (forbidEvalEnv dt) txt)
--
-- data SoundSelectErr = SoundSelectErr
--   deriving stock (Eq, Ord, Show)
--
-- instance Exception SoundSelectErr
--
-- soundSel :: (Pattern f) => Select -> f Attrs -> Either (InterpErr SoundSelectErr) (f Attrs)
-- soundSel sel attrs =
--   case sel of
--     SelectSample n -> Right (fmap (Map.insert "note" (DatumInt32 (fromIntegral n))) attrs)
--     _ -> Left (InterpErrEmbed SoundSelectErr)
--
-- soundProj :: Ident -> Attrs
-- soundProj = Map.singleton "sound" . DatumString . unIdent
--
-- soundInterpEnv :: InterpEnv SoundSelectErr Ident Attrs
-- soundInterpEnv = InterpEnv soundSel soundProj
--
-- soundEvalEnv :: EvalEnv SoundSelectErr Attrs
-- soundEvalEnv = EvalEnv soundInterpEnv identP
--
-- -- TODO parse sound with note selector here instead of having it in the grammar
-- liveEvalSoundPat :: (Pattern f) => Text -> f Attrs
-- liveEvalSoundPat txt = either (pure patEmpty) id (evalPat soundEvalEnv txt)
--
-- noteProjFn :: Integer -> Attrs
-- noteProjFn = Map.singleton "note" . DatumInt32 . fromInteger
--
-- data NoteErr = NoteErr
--   deriving stock (Eq, Ord, Show)
--
-- instance Exception NoteErr
--
-- linNoteP :: P LinNote
-- linNoteP = error "TODO"
--
-- noteProj :: LinNote -> Attrs
-- noteProj = error "TODO"
--
-- noteInterpEnv :: InterpEnv NoteErr LinNote Attrs
-- noteInterpEnv = InterpEnv forbidSel noteProj
--
-- noteEvalEnv :: EvalEnv NoteErr Attrs
-- noteEvalEnv = EvalEnv noteInterpEnv linNoteP
--
-- liveEvalNotePat :: (Pattern f) => Text -> f Attrs
-- liveEvalNotePat txt = either (pure patEmpty) id (evalPat noteEvalEnv txt)
