module Util where

import Control.Exception
import Data.ByteString.Lazy
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
    catchIOError action
        (\e ->
            if IOErr.isPermissionError e
            then (die $ "fatal: " ++ (displayException e))
            else IOErr.ioError e
        )
