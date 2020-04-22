module Command.Add
    ( doAdd
    ) where

import Control.Monad (foldM)
import Control.Monad.Extra (concatMapM)
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error

import Command.Base
import Database
import Database.Blob
import Index
import Repository
import Util
import Workspace

doAdd :: CommandBase -> IO ()
doAdd env = do
    let args = commArgs env
    let initRepo = mkRepository $ (commDir env) </> ".git"
    (initIndex, repo) <- catchGuardedIOError
        (getRepoWriteIndex initRepo)
        isAlreadyExistsError
        (\e -> do
            hPutStrLn (commStderr env) $ unlines
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
            hPutStrLn (commStderr env) $ "fatal: " ++ (ioeGetActualErrorString e)
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
            hPutStrLn (commStderr env) $ "error: " ++ (ioeGetActualErrorString e)
            hPutStrLn (commStderr env) "fatal: adding files failed"
            releaseIndexLock initIndex
            exitWith $ ExitFailure 128)
    tryWriteIndex needsWrite index
