module Command.Base
    ( CommandBase(..)
    , commAbsolutePath
    , commRepository
    ) where

import qualified Data.Map.Strict as Map
import System.FilePath
import System.IO (Handle)

import Repository

data CommandBase = CommandBase
    { commDir :: FilePath
    , commEnv :: Map.Map String String
    , commArgs :: [String]
    , commStdin :: Handle
    , commStdout :: Handle
    , commStderr :: Handle
    }

commAbsolutePath :: CommandBase -> FilePath -> FilePath
commAbsolutePath env path =
    normalise $
        if isAbsolute path
            then path
            else (commDir env) </> path

commRepository :: CommandBase -> Repository
commRepository env = mkRepository $ (commDir env) </> ".git"
