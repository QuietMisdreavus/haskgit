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
    , isDoesNotExistError
    , isPermissionError
    , isAlreadyExistsError)

import Database
import Database.Author
import Database.Blob
import Database.Commit
import Database.Tree
import Index
import Refs
import Repository
import Util
import Workspace

main :: IO ()
main = printErrorString $ getArgs >>= parseCommand

printErrorString :: IO () -> IO ()
printErrorString action = catchIOError action
    (\e -> putStrLn $ "fatal: " ++ (ioeGetActualErrorString e))

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
    let rootMsg = if (isNothing parent) then "(root-commit " else ""
    putStrLn $ "[" ++ rootMsg ++ (objectIdStr commit) ++ "]"
    putStrLn $ head $ lines message

doAdd :: [String] -> IO ()
doAdd args = do
    initRepo <- mkRepository <$> (</> ".git") <$> getCurrentDirectory
    (initIndex, repo) <- catchGuardedIOError
        (getRepoWriteIndex initRepo)
        isAlreadyExistsError
        (\e -> do
            hPutStrLn stderr $ unlines
                [ "fatal: " ++ (ioeGetActualErrorString e)
                , ""
                , "Another haskgit process seems to be running in this repository."
                , "Please make sure all processes are terminated then try again."
                , "If it still fails, a haskgit process may have crashed in this"
                , "repository earlier; remove the file manually to continue."
                ]
            exitWith $ ExitFailure 128)
    let dbPath = repoDBPath repo
    let ws = repoWSPath repo
    files <- catchGuardedIOError
        (concatMapM (listFileInWorkspace ws) args)
        isDoesNotExistError
        (\e -> do
            hPutStrLn stderr $ "fatal: " ++ (ioeGetActualErrorString e)
            releaseIndexLock initIndex
            exitWith $ ExitFailure 128)
    (index, needsWrite) <- catchGuardedIOError
        (foldM
            (\(i, wasUpdated) p -> do
                fileData <- readWorkspaceFile ws p
                fileStat <- fullStatWorkspaceFile ws p
                let obj = mkObject $ Blob fileData
                writeObject dbPath obj
                let (nextIndex, changed) = addIndexEntry p (objectId obj) fileStat i
                return (nextIndex, wasUpdated || changed))
            (initIndex, False)
            files)
        isPermissionError
        (\e -> do
            hPutStrLn stderr $ "error: " ++ (ioeGetActualErrorString e)
            hPutStrLn stderr "fatal: adding files failed"
            releaseIndexLock initIndex
            exitWith $ ExitFailure 128)
    tryWriteIndex needsWrite index
