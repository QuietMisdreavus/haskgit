module Repository
    ( Repository(repoGitPath)
    , mkRepository
    , repoDBPath
    , repoWSPath
    , getRepoWriteIndex
    , getRepoReadIndex
    ) where

import System.FilePath

import Index

data Repository = Repository
    { repoGitPath :: FilePath
    , repoIndex :: Maybe Index
    }

emptyRepository :: Repository
emptyRepository = Repository
    { repoGitPath = ""
    , repoIndex = Nothing
    }

mkRepository :: FilePath -> Repository
mkRepository gitPath = emptyRepository { repoGitPath = gitPath }

repoDBPath :: Repository -> FilePath
repoDBPath repo = (repoGitPath repo) </> "objects"

repoWSPath :: Repository -> FilePath
repoWSPath repo = takeDirectory $ repoGitPath repo

repoIndexPath :: Repository -> FilePath
repoIndexPath repo = (repoGitPath repo) </> "index"

getRepoWriteIndex :: Repository -> IO (Index, Repository)
getRepoWriteIndex repo =
    case (repoIndex repo) of
        Just (i @ (WriteIndex _ _)) -> return (i, repo)
        _ -> do
            i <- loadIndexToWrite $ repoIndexPath repo
            return (i, repo { repoIndex = Just i })

getRepoReadIndex :: Repository -> IO (Index, Repository)
getRepoReadIndex repo =
    case (repoIndex repo) of
        Just i -> return (i, repo)
        Nothing -> do
            i <- loadIndexToRead $ repoIndexPath repo
            return (i, repo { repoIndex = Just i })
