module Refs (updateHead, readHead) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Util.Hash (ObjectId, hexDigest)
import Lockfile

headPath :: String -> String
headPath gitPath = gitPath </> "HEAD"

updateHead :: String -> ObjectId -> IO ()
updateHead gitPath oid = do
    let h = headPath gitPath
    lockfile <- mkLockfile h
    case lockfile of
        Left _ -> ioError (userError $ "Could not acquire lock on file: " ++ h)
        Right lock -> do
            writeLockfile lock (hexDigest oid)
            writeLockfile lock "\n"
            commitLock lock
    -- writeFile (headPath gitPath) (hexDigest oid)

readHead :: String -> IO (Maybe String)
readHead gitPath = do
    let h = headPath gitPath
    hasHead <- doesFileExist h
    if (hasHead)
        then Just <$> dropWhileEnd isSpace <$> readFile h
        else pure $ Nothing
