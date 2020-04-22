module Command.Status
    ( doStatus
    ) where

import Control.Monad (forM_, foldM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.FilePath (addTrailingPathSeparator)
import System.IO (hPutStrLn)
import System.Posix.Files (isDirectory)

import Command.Base
import Index
import Repository
import Workspace

doStatus :: CommandBase -> IO ()
doStatus env = do
    (index, repo) <- getRepoReadIndex $ commRepository env
    let ws = repoWSPath repo
    untracked <- scanWorkspace index Set.empty ws ""
    forM_ untracked (\f -> hPutStrLn (commStdout env) $ "?? " ++ f)

scanWorkspace :: Index -> Set.Set FilePath -> FilePath -> FilePath -> IO (Set.Set FilePath)
scanWorkspace index untracked ws prefix = do
    dirContents <- listDirInWorkspace ws prefix
    foldM
        (\s (path, stat) ->
            case (indexHasEntry index path, isDirectory stat) of
              (True, True) -> scanWorkspace index s ws path
              (True, False) -> return s
              (False, isDir) ->
                  let finalPath = if isDir then addTrailingPathSeparator path else path
                   in return $ Set.insert finalPath s)
        untracked
        (Map.toList dirContents)
