-- {-# LANGUAGE OverloadedStrings #-}

module Minipat.Dirt.Eval
  ( datumPat
  )
where

-- ( liveEvalPat
-- , liveEvalSoundPat
-- , liveEvalNotePat
-- )

import Control.Exception (Exception)
import Dahdit.Midi.Osc (Datum (..), DatumType (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import Looksee (intP, sciP)
import Minipat.Ast (Ident (..), Pattern (..), Select (..))
import Minipat.Dirt.Notes (LinNote)
import Minipat.Dirt.Osc (Attrs, DatumProxy (..))
import Minipat.Eval (evalPat)
import Minipat.Interp (InterpErr (..))
import Minipat.Parser (P, identP)

datumP :: DatumProxy a -> P a
datumP = \case
  DatumProxyInt32 -> fmap fromInteger intP
  DatumProxyInt64 -> fmap fromInteger intP
  DatumProxyFloat -> fmap realToFrac sciP
  DatumProxyDouble -> fmap realToFrac sciP
  DatumProxyString -> fmap unIdent identP

-- TODO figure out out to propagate error
parsePat :: (Pattern f) => P a -> Text -> f a
parsePat p = either (pure patEmpty) id . evalPat p

datumPat :: (Pattern f) => DatumProxy a -> Text -> f a
datumPat = parsePat . datumP

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
