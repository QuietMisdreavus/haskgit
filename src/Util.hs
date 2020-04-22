module Util
    ( GitObject(..)
    , dieOnPermError
    , catchGuardedIOError
    , throwCustomIOError
    , ioeGetActualErrorString
    , foldWhileM
    ) where

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

-- `mkCustomIOError type msg` creates an `IOError` with the given type and error string.
-- the error will not have a location set.
mkCustomIOError :: IOErr.IOErrorType -> String -> IOErr.IOError
mkCustomIOError eType eMsg =
    IOErr.ioeSetErrorString
        (IOErr.mkIOError eType "" Nothing Nothing)
        eMsg

-- `throwCustomIOError type msg` creates an `IOError` with the given type and error
-- string, then immediately throws it with `ioError`.
throwCustomIOError :: IOErr.IOErrorType -> String -> IO a
throwCustomIOError eType eMsg = ioError $ mkCustomIOError eType eMsg

-- `ioeGetActualErrorString err` returns the internal "description" field normally set by
-- `ioeGetErrorString`, even for non-user errors.
ioeGetActualErrorString :: IOErr.IOError -> String
ioeGetActualErrorString ioe =
    IOErr.ioeGetErrorString $ IOErr.ioeSetErrorType ioe IOErr.userErrorType

-- `foldWhileM test seed action` checks `seed` against `test`, and returns it if `test
-- seed` fails. otherwise, it executes `action`, concatenates it to the end of `seed`, and
-- tries again.
foldWhileM :: Monad m => (a -> Bool) -> a -> (a -> m a) -> m a
foldWhileM test seed action = if (not $ test seed) then return seed else do
    next <- action seed
    foldWhileM test next action
