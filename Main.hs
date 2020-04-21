module Main where

import Control.Monad ((<=<))
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Time.LocalTime (getZonedTime)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO (getContents)

import Author
import Blob
import Commit
import Database
import Entry
import Refs
import Tree
import Util
import Workspace

main :: IO ()
main = getArgs >>= parseCommand

parseCommand :: [String] -> IO ()
parseCommand ("init":xs) = doInit $ listToMaybe xs
parseCommand ("commit":_) = doCommit
parseCommand (comm:_) = commandError comm
parseCommand [] = commandError ""

commandError :: String -> IO ()
commandError comm = die $ "'" ++ comm ++ "' is not a command."

-- performs an `init` command: creates an initial `.git` folder in the given directory.
-- defaults to the current directory if no arguments given.
doInit :: Maybe String -> IO ()
doInit Nothing = doInit =<< pure <$> getCurrentDirectory
doInit (Just path) = do
    fullPath <- makeAbsolute path
    let gitPath = fullPath </> ".git"
    foldMap
        (dieOnPermError . (createDirectoryIfMissing True))
        (map (gitPath </>) ["objects", "refs"])
    putStrLn $ "Initialized empty haskgit repository in " ++ gitPath

doCommit :: IO ()
doCommit = do
    rootPath <- getCurrentDirectory
    let gitPath = rootPath </> ".git"
    let dbPath = gitPath </> "objects"
    fileList <- listWorkspaceFiles rootPath
    entries <- mapM
        (\path -> do
            fileData <- readWorkspaceFile rootPath path
            let obj = mkObject $ Blob fileData
            writeObject dbPath obj
            return $ Entry path $ objectIdBStr obj)
        fileList
    let tree = mkObject $ Tree entries
    writeObject dbPath tree
    parent <- readHead gitPath
    name <- fromMaybe "" <$> lookupEnv "GIT_AUTHOR_NAME"
    email <- fromMaybe "" <$> lookupEnv "GIT_AUTHOR_EMAIL"
    author <- (Author name email) <$> getZonedTime
    message <- getContents
    let commit = mkObject $ Commit (objectId tree) parent author message
    writeObject dbPath commit
    updateHead gitPath $ objectId commit
    let rootMsg = if (isNothing parent) then "(root-commit " else ""
    putStrLn $ "[" ++ rootMsg ++ (objectIdStr commit) ++ "]"
    putStrLn $ head $ lines message
