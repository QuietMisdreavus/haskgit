module Refs (updateHead, readHead) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Util.Hash (ObjectId, hexDigest)
import Lockfile

headPath :: FilePath -> FilePath
headPath gitPath = gitPath </> "HEAD"

updateHead :: FilePath -> ObjectId -> IO ()
updateHead gitPath oid = do
    let h = headPath gitPath
    lock <- mkLockfile h
    writeLockfile lock (hexDigest oid)
    writeLockfile lock "\n"
    commitLock lock

readHead :: FilePath -> IO (Maybe String)
readHead gitPath = do
    let h = headPath gitPath
    hasHead <- doesFileExist h
    if (hasHead)
        then Just <$> dropWhileEnd isSpace <$> readFile h
        else pure $ Nothing
