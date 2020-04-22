module Command.Init
    ( doInit
    ) where

import Data.Maybe
import System.Directory
import System.FilePath
import System.IO

import Command.Base
import Util

-- performs an `init` command: creates an initial `.git` folder in the given directory.
-- defaults to the current directory if no arguments given.
doInit :: CommandBase -> IO ()
doInit env = do
    let path = fromMaybe (commDir env) $ listToMaybe $ commArgs env
    let fullPath = commAbsolutePath env path
    let gitPath = fullPath </> ".git"
    foldMap
        (dieOnPermError . createDirectoryIfMissing True . (gitPath </>))
        ["objects", "refs"]
    hPutStrLn (commStdout env) $ "Initialized empty haskgit repository in " ++ gitPath
