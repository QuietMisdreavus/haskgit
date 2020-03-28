module Workspace (listWorkspaceFiles, readWorkspaceFile) where

import Prelude hiding (readFile)

import Data.ByteString.Lazy (ByteString, readFile)
import Data.List
import System.Directory
import System.FilePath

-- a list of filenames that should be excluded from `listWorkspaceFiles`.
ignoreFilenames :: [String]
ignoreFilenames = [".", "..", ".git"]

-- filters the given filename list for files that should not be saved in git.
filterFilenames :: [String] -> [String]
filterFilenames dir = filter (\x -> notElem x ignoreFilenames) dir

-- filters the given filenames to exclude vim swap files.
filterSwapFiles :: [String] -> [String]
filterSwapFiles list = filter (\x -> not $ isSuffixOf ".swp" x) list

-- returns the names of the files in the given workspace directory.
listWorkspaceFiles :: String -> IO [String]
listWorkspaceFiles path =
    filterSwapFiles <$>
    filterFilenames <$>
    getDirectoryContents path

-- from the given path, loads the given file.
readWorkspaceFile :: String -> String -> IO ByteString
readWorkspaceFile ws f = readFile (ws </> f)
