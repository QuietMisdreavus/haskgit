module Main (main) where

import Control.Monad (foldM)
import Control.Monad.Extra (concatMapM)
import Data.Maybe
import Data.Time.LocalTime (getZonedTime)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
    ( catchIOError
    , ioeGetErrorString
    , isDoesNotExistErrorType
    , ioeGetErrorType)

import Database
import Database.Author
import Database.Blob
import Database.Commit
import Database.Tree
import Index
import Refs
import Util
import Workspace

main :: IO ()
main = printErrorString $ getArgs >>= parseCommand

printErrorString :: IO () -> IO ()
printErrorString action = catchIOError action
    (\e -> putStrLn $ "fatal: " ++ (ioeGetErrorString e))

parseCommand :: [String] -> IO ()
parseCommand ("init":xs) = doInit $ listToMaybe xs
parseCommand ("add":xs) = doAdd xs
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
    let indexPath = gitPath </> "index"
    initIndex <- loadIndexToRead indexPath
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
    let rootMsg = if (isNothing parent) then "(root-commit " else ""
    putStrLn $ "[" ++ rootMsg ++ (objectIdStr commit) ++ "]"
    putStrLn $ head $ lines message

doAdd :: [String] -> IO ()
doAdd args = do
    rootPath <- getCurrentDirectory
    let gitPath = rootPath </> ".git"
    let dbPath = gitPath </> "objects"
    let indexPath = gitPath </> "index"
    initIndex <- loadIndexToWrite indexPath
    files <- catchGuardedIOError
        (concatMapM (listFileInWorkspace rootPath) args)
        (isDoesNotExistErrorType . ioeGetErrorType) (\e -> do
            hPutStrLn stderr $ "fatal: " ++ (ioeGetActualErrorString e)
            releaseIndexLock initIndex
            exitWith $ ExitFailure 128)
    (index, needsWrite) <- foldM
        (\(i, wasUpdated) p -> do
            fileData <- readWorkspaceFile rootPath p
            fileStat <- fullStatWorkspaceFile rootPath p
            let obj = mkObject $ Blob fileData
            writeObject dbPath obj
            let (nextIndex, changed) = addIndexEntry p (objectId obj) fileStat i
            return (nextIndex, wasUpdated || changed))
        (initIndex, False)
        files
    tryWriteIndex needsWrite index
