module Main (main) where

import qualified Data.Map.Strict as Map
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.IO.Error

import Command
import Command.Base
import Util

main :: IO ()
main = exitWith =<< catchGuardedIOError'
    (do
        curDir <- getCurrentDirectory
        env <- Map.fromList <$> getEnvironment
        args <- getArgs
        let comm = CommandBase curDir env args stdin stdout stderr
        runCommand comm)
    [ (isUserError,
        (\e -> do
            hPutStrLn stderr $ "haskgit: " ++ (ioeGetActualErrorString e)
            exitWith $ ExitFailure 1))
    , (const True,
        (\e -> do
            hPutStrLn stderr $ "fatal: " ++ (ioeGetActualErrorString e)
            exitWith $ ExitFailure 1))
    ]
