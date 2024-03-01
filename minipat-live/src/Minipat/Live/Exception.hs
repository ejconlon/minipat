-- | Just some junk to handle non-async exceptions.
module Minipat.Live.Exception
  ( isUserErr
  , catchUserErr
  )
where

import Control.Exception (Exception (..), SomeAsyncException (..), SomeException)
import Control.Monad.Catch (catchIf)

isUserErr :: SomeException -> Bool
isUserErr x =
  case fromException x of
    Just (SomeAsyncException _) -> False
    _ -> True

catchUserErr :: IO a -> (SomeException -> IO a) -> IO a
catchUserErr = catchIf isUserErr
