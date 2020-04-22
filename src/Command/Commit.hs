module Command.Commit
    ( doCommit
    ) where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Time.LocalTime (getZonedTime)
import System.FilePath
import System.IO

import Command.Base
import Database
import Database.Author
import Database.Commit
import Database.Tree
import Index
import Refs
import Repository

doCommit :: CommandBase -> IO ()
doCommit env = do
    let initRepo = mkRepository $ (commDir env) </> ".git"
    (initIndex, repo) <- getRepoReadIndex initRepo
    let gitPath = repoGitPath repo
    let dbPath = repoDBPath repo
    let tree = buildTree $ indexEntries initIndex
    (treeId, _) <- traverseTree (writeObject dbPath) tree
    parent <- readHead gitPath
    let name = Map.findWithDefault "" "GIT_AUTHOR_NAME" $ commEnv env
    let email = Map.findWithDefault "" "GIT_AUTHOR_EMAIL" $ commEnv env
    author <- (Author name email) <$> getZonedTime
    message <- hGetContents $ commStdin env
    let commit = mkObject $ Commit treeId parent author message
    writeObject dbPath commit
    updateHead gitPath $ objectId commit
    let rootMsg = if (isNothing parent) then "(root-commit) " else ""
    hPutStrLn (commStdout env) $ "[" ++ rootMsg ++ (objectIdStr commit) ++ "]"
    hPutStrLn (commStdout env) $ head $ lines message
