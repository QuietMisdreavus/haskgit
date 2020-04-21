module Lockfile where

import Data.List (isSuffixOf)
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error (ioError, userError)

data Lockfile = Lockfile String Handle

lockFileName :: String -> String
lockFileName filename = filename <.> "lock"

-- TODO: this locking implementation is racy, but openFile doesn't allow using the EXCL
-- flag :/

mkLockfile :: String -> IO (Either () Lockfile)
mkLockfile filename = do
    let lockName = lockFileName filename
    lockExists <- doesFileExist lockName
    if lockExists
        then return $ Left ()
        else Right <$> Lockfile filename <$> openFile lockName ReadWriteMode

writeLockfile :: Lockfile -> String -> IO ()
writeLockfile (Lockfile filename lockHandle) outStr = do
    holdingLock <- hIsOpen lockHandle
    if not holdingLock
        then ioError (userError $ "Not holding lock on file: " ++ (lockFileName filename))
        else hPutStr lockHandle outStr

commitLock :: Lockfile -> IO ()
commitLock (Lockfile filename lockHandle) = do
    holdingLock <- hIsOpen lockHandle
    if not holdingLock
        then ioError (userError $ "Not holding lock on file: " ++ (lockFileName filename))
        else do
            hClose lockHandle
            let lockName = lockFileName filename
            renameFile lockName filename
