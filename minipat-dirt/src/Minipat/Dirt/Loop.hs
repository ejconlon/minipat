module Minipat.Dirt.Loop
  ( NonPosTimeDeltaErr (..)
  , loopAsync
  )
where

import Control.Concurrent.Async (Async)
import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Minipat.Dirt.Ref (Ref, ReleaseVar, refAsync)
import Nanotime (MonoTime (..), TimeDelta (..), awaitDelta, currentTime)

data NonPosTimeDeltaErr = NonPosTimeDeltaErr
  deriving stock (Eq, Ord, Show)

instance Exception NonPosTimeDeltaErr

awaitTime :: TimeDelta -> IORef MonoTime -> IO ()
awaitTime td tv = do
  lastTime <- readIORef tv
  if lastTime == MonoTime 0
    then do
      curTime <- currentTime
      writeIORef tv curTime
    else do
      nextTime <- awaitDelta lastTime td
      writeIORef tv nextTime

loopAsync :: ReleaseVar -> IORef TimeDelta -> IO (Maybe a) -> IO (Ref (Async a))
loopAsync rv tdv act = do
  tv <- newIORef (MonoTime 0)
  let act' = do
        td@(TimeDelta x) <- readIORef tdv
        unless (x > 0) (throwIO NonPosTimeDeltaErr)
        awaitTime td tv
        act >>= maybe act' pure
  refAsync rv act'
