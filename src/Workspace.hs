module Workspace (
    listWorkspaceFiles,
    listFileInWorkspace,
    readWorkspaceFile,
    statWorkspaceFile,
    fullStatWorkspaceFile,
    allFilters
) where

import Prelude hiding (readFile)

import Control.Monad.Extra (concatMapM)
import Data.ByteString.Lazy (ByteString, readFile)
import Data.List
import System.Directory
import System.FilePath
import System.Posix.Files (getFileStatus, FileStatus)

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
listWorkspaceFiles path = listFileInWorkspace path "."

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

listFileInWorkspace :: String -> String -> IO [String]
listFileInWorkspace ws file = do
    let path = if file == "." then ws else ws </> file
    isDir <- doesDirectoryExist path
    if isDir
        then do
            files <- listWorkspaceFiles' path
            concatMapM (listFileInWorkspace ws) files
        else
            pure [makeRelative ws file]

-- from the given path, loads the given file.
readWorkspaceFile :: String -> String -> IO ByteString
readWorkspaceFile ws f = readFile (ws </> f)

-- from the given path, reads the file permissions for the given file.
statWorkspaceFile :: String -> String -> IO Permissions
statWorkspaceFile ws f = getPermissions (ws </> f)

fullStatWorkspaceFile :: String -> String -> IO FileStatus
fullStatWorkspaceFile ws f = getFileStatus (ws </> f)
