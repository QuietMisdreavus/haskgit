module Refs (updateHead, readHead) where

import Data.Char (isSpace)
import Data.Digest.Pure.SHA (showDigest)
import Data.List (dropWhileEnd)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (writeFile, readFile)
import System.IO.Error (ioError, userError)

import Database (ObjectId)
import Lockfile

headPath :: String -> String
headPath gitPath = gitPath </> "HEAD"

updateHead :: String -> ObjectId -> IO ()
updateHead gitPath oid = do
    let head = headPath gitPath
    lockfile <- mkLockfile head
    case lockfile of
        Left _ -> ioError (userError $ "Could not acquire lock on file: " ++ head)
        Right lock -> do
            writeLockfile lock (showDigest oid)
            writeLockfile lock "\n"
            commitLock lock
    -- writeFile (headPath gitPath) (showDigest oid)

readHead :: String -> IO (Maybe String)
readHead gitPath = do
    let head = headPath gitPath
    hasHead <- doesFileExist head
    if (hasHead)
        then Just <$> dropWhileEnd isSpace <$> readFile head
        else pure $ Nothing
