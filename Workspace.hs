module Workspace (
    listWorkspaceFiles,
    listWorkspaceFiles', -- TODO make private
    readEntry,
    readWorkspaceFile,
    statWorkspaceFile
) where

import Prelude hiding (readFile)

import Control.Monad.Extra (concatMapM)
import Data.ByteString.Lazy (ByteString, readFile)
import Data.List
import System.Directory
import System.FilePath

import Blob
import Database
import Entry

-- a list of filenames that should be excluded from `listWorkspaceFiles`.
ignoreFilenames :: [String]
ignoreFilenames = [".", "..", ".git", "tags", ".stack-work"]

-- filters the given filename list for files that should not be saved in git.
filterFilenames :: [String] -> [String]
filterFilenames dir = filter (\x -> notElem x ignoreFilenames) dir

-- filters the given filenames to exclude vim swap files.
filterSwapFiles :: [String] -> [String]
filterSwapFiles list = filter (\x -> not $ isSuffixOf ".swp" x) list

-- combines all the previous filename filters into one call
allFilters :: [String] -> [String]
allFilters list = filterSwapFiles $ filterFilenames list

-- returns the names of the files in the given workspace directory.
listWorkspaceFiles :: String -> IO [String]
listWorkspaceFiles path =
    (fmap . fmap) (makeRelative path) (listWorkspaceFiles' path)

listWorkspaceFiles' :: String -> IO [String]
listWorkspaceFiles' path = do
    filenames <- allFilters <$> getDirectoryContents path
    concatMapM
        (\n -> do
            let fpath = path </> n
            isDir <- doesDirectoryExist fpath
            if isDir
                then listWorkspaceFiles' fpath
                else pure [fpath]
        )
        filenames

-- from the given path, loads the given file.
readWorkspaceFile :: String -> String -> IO ByteString
readWorkspaceFile ws f = readFile (ws </> f)

-- from the given path, reads the file permissions for the given file.
statWorkspaceFile :: String -> String -> IO Permissions
statWorkspaceFile ws f = getPermissions (ws </> f)

readEntry :: String -> String -> IO Entry
readEntry ws path = do
    fileData <- readWorkspaceFile ws path
    fileStat <- statWorkspaceFile ws path
    let obj = mkObject $ Blob fileData
    return $ Entry path (objectId obj) fileStat
