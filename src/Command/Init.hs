module Command.Init
    ( doInit
    ) where

import System.Directory
import System.FilePath

import Util

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
