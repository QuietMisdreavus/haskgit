module Lockfile
    ( Lockfile(..)
    , mkLockfile
    , mkBinLockfile
    , writeLockfile
    , writeLockfileBStr
    , writeLockfileBuf
    , commitLock
    , rollbackLock
    ) where

import qualified Data.ByteString.Builder as ByteBuf
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BStrC
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error

import Util

data Lockfile = Lockfile FilePath Handle

lockFileName :: FilePath -> FilePath
lockFileName filename = filename <.> "lock"

-- TODO: this locking implementation is racy, but openFile doesn't allow using the EXCL
-- flag :/

mkLockfile :: FilePath -> IO Lockfile
mkLockfile filename = do
    let lockName = lockFileName filename
    throwIfLockExists lockName
    Lockfile filename <$> openFile lockName ReadWriteMode

mkBinLockfile :: FilePath -> IO Lockfile
mkBinLockfile filename = do
    let lockName = lockFileName filename
    throwIfLockExists lockName
    h <- openBinaryFile lockName ReadWriteMode
    hSetBuffering h (BlockBuffering Nothing)
    return $ Lockfile filename h

throwIfLockExists :: FilePath -> IO ()
throwIfLockExists lockName = do
    lockExists <- doesFileExist lockName
    if lockExists
        then throwCustomIOError
            alreadyExistsErrorType
            $ "Unable to create '" ++ lockName ++ "': File exists."
        else pure ()

writeLockfile :: Lockfile -> String -> IO ()
writeLockfile (Lockfile filename lockHandle) outStr = do
    holdingLock <- hIsOpen lockHandle
    if not holdingLock
        then ioError (userError $ "Not holding lock on file: " ++ lockFileName filename)
        else hPutStr lockHandle outStr

writeLockfileBStr :: Lockfile -> ByteString -> IO ()
writeLockfileBStr (Lockfile filename lockHandle) outStr = do
    holdingLock <- hIsOpen lockHandle
    if not holdingLock
        then ioError (userError $ "Not holding lock on file: " ++ lockFileName filename)
        else BStrC.hPutStr lockHandle outStr

writeLockfileBuf :: Lockfile -> ByteBuf.Builder -> IO ()
writeLockfileBuf (Lockfile filename lockHandle) outBuf = do
    holdingLock <- hIsOpen lockHandle
    if not holdingLock
        then ioError (userError $ "Not holding lock on file: " ++ lockFileName filename)
        else ByteBuf.hPutBuilder lockHandle outBuf

commitLock :: Lockfile -> IO ()
commitLock (Lockfile filename lockHandle) = do
    holdingLock <- hIsOpen lockHandle
    if not holdingLock
        then ioError (userError $ "Not holding lock on file: " ++ lockFileName filename)
        else do
            hClose lockHandle
            let lockName = lockFileName filename
            renameFile lockName filename

rollbackLock :: Lockfile -> IO ()
rollbackLock (Lockfile filename lockHandle) = do
    holdingLock <- hIsOpen lockHandle
    if not holdingLock
        then ioError (userError $ "Not holding lock on file: " ++ lockFileName filename)
        else do
            hClose lockHandle
            removeFile $ lockFileName filename
