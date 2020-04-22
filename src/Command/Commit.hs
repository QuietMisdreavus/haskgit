module Command.Commit
    ( doCommit
    ) where

import Data.Maybe
import Data.Time.LocalTime (getZonedTime)
import System.Directory
import System.Environment
import System.FilePath

import Database
import Database.Author
import Database.Commit
import Database.Tree
import Index
import Refs
import Repository

doCommit :: IO ()
doCommit = do
    initRepo <- mkRepository <$> (</> ".git") <$> getCurrentDirectory
    (initIndex, repo) <- getRepoReadIndex initRepo
    let gitPath = repoGitPath repo
    let dbPath = repoDBPath repo
    let tree = buildTree $ indexEntries initIndex
    (treeId, _) <- traverseTree (writeObject dbPath) tree
    parent <- readHead gitPath
    name <- fromMaybe "" <$> lookupEnv "GIT_AUTHOR_NAME"
    email <- fromMaybe "" <$> lookupEnv "GIT_AUTHOR_EMAIL"
    author <- (Author name email) <$> getZonedTime
    message <- getContents
    let commit = mkObject $ Commit treeId parent author message
    writeObject dbPath commit
    updateHead gitPath $ objectId commit
    let rootMsg = if (isNothing parent) then "(root-commit) " else ""
    putStrLn $ "[" ++ rootMsg ++ (objectIdStr commit) ++ "]"
    putStrLn $ head $ lines message
