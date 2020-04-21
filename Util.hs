module Util where

import Control.Exception
import Data.ByteString.Lazy hiding (find)
import Data.List (find)
import System.Exit
import System.IO.Error as IOErr

-- typeclass that describes a git object.
class GitObject a where
    gitData :: a -> ByteString
    gitType :: a -> String

-- catches an `IOException` of `isPermissionError`, prints a message to stderr, and exits.
-- all other exceptions are rethrown.
dieOnPermError :: IO a -> IO a
dieOnPermError action =
    catchGuardedIOError action
        IOErr.isPermissionError
        (\e -> die $ "fatal: " ++ (displayException e))

-- `catchGuardedIOError action guard catch` performs `action`, and catches exceptions
-- thrown that match `guard` by executing `catch`.
catchGuardedIOError :: IO a -> (IOErr.IOError -> Bool) -> (IOErr.IOError -> IO a) -> IO a
catchGuardedIOError action guard catcher =
    catchGuardedIOError' action [(guard, catcher)]

-- `catchGuardedIOError' action guards` is like `catchGuardedIOError`, but allows for
-- multiple exception handlers. the first matching guard will be executed, otherwise an
-- exception will be rethrown.
catchGuardedIOError' ::
    IO a -> [((IOErr.IOError -> Bool), (IOErr.IOError -> IO a))] -> IO a
catchGuardedIOError' action guards =
    catchIOError action
        (\e -> case (find (\(guard, _) -> guard e) guards) of
            Just (_, catcher) -> catcher e
            Nothing -> IOErr.ioError e)
