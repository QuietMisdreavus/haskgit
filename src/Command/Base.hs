module Command.Base
    ( CommandBase(..)
    , commAbsolutePath
    ) where

import qualified Data.Map.Strict as Map
import System.FilePath
import System.IO (Handle)

data CommandBase = CommandBase
    { commDir :: String
    , commEnv :: Map.Map String String
    , commArgs :: [String]
    , commStdin :: Handle
    , commStdout :: Handle
    , commStderr :: Handle
    }

commAbsolutePath :: CommandBase -> String -> String
commAbsolutePath env path =
    normalise $
        if isAbsolute path
            then path
            else (commDir env) </> path
