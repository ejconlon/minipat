{-# LANGUAGE OverloadedStrings #-}

-- | Logging via `little-logger`/`monad-logger`
module Minipat.Live.Logger
  ( LogAction
  , LogLevel (..)
  , newLogger
  , nullLogger
  , logLvl
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logException
  )
where

import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (Exception (..))
import Data.Text (Text)
import Data.Text qualified as T
import LittleLogger (LogAction (..), LogLevel (..), defaultLogAction, logOtherN, runLogActionM)

newLogger :: IO LogAction
newLogger = do
  let logger = defaultLogAction
  lock <- newMVar ()
  pure (LogAction (\loc src lvl msg -> withMVar lock (\_ -> unLogAction logger loc src lvl msg)))

nullLogger :: LogAction
nullLogger = LogAction (\_ _ _ _ -> pure ())

logLvl :: LogAction -> LogLevel -> Text -> IO ()
logLvl logger lvl msg = runLogActionM (logOtherN lvl msg) logger

logDebug :: LogAction -> Text -> IO ()
logDebug logger = logLvl logger LevelDebug

logInfo :: LogAction -> Text -> IO ()
logInfo logger = logLvl logger LevelInfo

logWarn :: LogAction -> Text -> IO ()
logWarn logger = logLvl logger LevelWarn

logError :: LogAction -> Text -> IO ()
logError logger = logLvl logger LevelError

logException :: (Exception e) => LogAction -> Text -> e -> IO ()
logException logger msg err = logError logger (msg <> ":\n" <> T.pack (displayException err))
