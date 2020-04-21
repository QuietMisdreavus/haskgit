module Refs (updateHead, readHead) where

import Data.Char (isSpace)
import Data.Digest.Pure.SHA (showDigest)
import Data.List (dropWhileEnd)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (writeFile, readFile)

import Database (ObjectId)

headPath :: String -> String
headPath gitPath = gitPath </> "HEAD"

updateHead :: String -> ObjectId -> IO ()
updateHead gitPath oid = writeFile (headPath gitPath) (showDigest oid)

readHead :: String -> IO (Maybe String)
readHead gitPath = do
    let head = headPath gitPath
    hasHead <- doesFileExist head
    if (hasHead)
        then Just <$> dropWhileEnd isSpace <$> readFile head
        else pure $ Nothing
