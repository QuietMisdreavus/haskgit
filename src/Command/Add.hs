module Command.Add
    ( doAdd
    ) where

import Control.Monad (foldM)
import Control.Monad.Extra (concatMapM)
import System.Exit
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
    (initIndex, repo) <- tryGetIndex env
    (index, needsWrite) <- catchGuardedIOError
        ((foldM (addFileToIndex repo) (initIndex, False))
            =<< expandedPaths env repo)
        isPermissionError
        (handleUnreadableFile env initIndex)
    tryWriteIndex needsWrite index

tryGetIndex :: CommandBase -> IO (Index, Repository)
tryGetIndex env = catchGuardedIOError
    (getRepoWriteIndex $ commRepository env)
    isAlreadyExistsError
    (handleLockedIndex env)

handleLockedIndex :: CommandBase -> IOError -> IO a
handleLockedIndex env e = do
    let currentStderr = commStderr env
    hPutStrLn currentStderr $ unlines
        [ "fatal: " ++ (ioeGetActualErrorString e)
        , ""
        , "Another haskgit process seems to be running in this repository."
        , "Please make sure all processes are terminated then try again."
        , "If it still fails, a haskgit process may have crashed in this"
        , "repository earlier; remove the file manually to continue."
        ]
    exitWith $ ExitFailure 128

handleMissingFile :: CommandBase -> Index -> IOError -> IO a
handleMissingFile env index e = do
    let currentStderr = commStderr env
    hPutStrLn currentStderr $ "fatal: " ++ (ioeGetActualErrorString e)
    releaseIndexLock index
    exitWith $ ExitFailure 128

handleUnreadableFile :: CommandBase -> Index -> IOError -> IO a
handleUnreadableFile env index e = do
    let currentStderr = commStderr env
    hPutStrLn currentStderr $ "error: " ++ (ioeGetActualErrorString e)
    hPutStrLn currentStderr "fatal: adding files failed"
    releaseIndexLock index
    exitWith $ ExitFailure 128

expandedPaths :: CommandBase -> Repository -> IO [FilePath]
expandedPaths env repo = do
    let args = commArgs env
    let ws = repoWSPath repo
    (index, _) <- getRepoWriteIndex repo
    catchGuardedIOError
        (concatMapM (listFileInWorkspace ws) args)
        isDoesNotExistError
        (handleMissingFile env index)

addFileToIndex :: Repository -> (Index, Bool) -> FilePath -> IO (Index, Bool)
addFileToIndex repo (i, wasUpdated) p = do
    let dbPath = repoDBPath repo
    let ws = repoWSPath repo
    fileData <- readWorkspaceFile ws p
    fileStat <- fullStatWorkspaceFile ws p
    let obj = mkObject $ Blob fileData
    writeObject dbPath obj
    let (nextIndex, changed) = addIndexEntry p (objectId obj) fileStat i
    return (nextIndex, wasUpdated || changed)
