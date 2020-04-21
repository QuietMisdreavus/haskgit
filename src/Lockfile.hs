module Lockfile where

import qualified Data.ByteString.Builder as ByteBuf
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BStrC
import System.Directory
import System.FilePath
import System.IO

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

mkBinLockfile :: String -> IO (Either () Lockfile)
mkBinLockfile filename = do
    let lockName = lockFileName filename
    lockExists <- doesFileExist lockName
    if lockExists
        then return $ Left ()
        else do
            h <- openBinaryFile lockName ReadWriteMode
            hSetBuffering h (BlockBuffering Nothing)
            return $ Right $ Lockfile filename h

writeLockfile :: Lockfile -> String -> IO ()
writeLockfile (Lockfile filename lockHandle) outStr = do
    holdingLock <- hIsOpen lockHandle
    if not holdingLock
        then ioError (userError $ "Not holding lock on file: " ++ (lockFileName filename))
        else hPutStr lockHandle outStr

writeLockfileBStr :: Lockfile -> ByteString -> IO ()
writeLockfileBStr (Lockfile filename lockHandle) outStr = do
    holdingLock <- hIsOpen lockHandle
    if not holdingLock
        then ioError (userError $ "Not holding lock on file: " ++ (lockFileName filename))
        else BStrC.hPutStr lockHandle outStr

writeLockfileBuf :: Lockfile -> ByteBuf.Builder -> IO ()
writeLockfileBuf (Lockfile filename lockHandle) outBuf = do
    holdingLock <- hIsOpen lockHandle
    if not holdingLock
        then ioError (userError $ "Not holding lock on file: " ++ (lockFileName filename))
        else ByteBuf.hPutBuilder lockHandle outBuf

commitLock :: Lockfile -> IO ()
commitLock (Lockfile filename lockHandle) = do
    holdingLock <- hIsOpen lockHandle
    if not holdingLock
        then ioError (userError $ "Not holding lock on file: " ++ (lockFileName filename))
        else do
            hClose lockHandle
            let lockName = lockFileName filename
            renameFile lockName filename
