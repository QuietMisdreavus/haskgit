module Main (main) where

import System.Environment
import System.Exit
import System.IO
import System.IO.Error

import Command
import Util

main :: IO ()
main = catchGuardedIOError'
    (parseCommand =<< getArgs)
    [ (isUserError,
        (\e -> do
            hPutStrLn stderr $ "haskgit: " ++ (ioeGetActualErrorString e)
            exitWith $ ExitFailure 1))
    , (const True,
        (\e -> do
            hPutStrLn stderr $ "fatal: " ++ (ioeGetActualErrorString e)
            exitWith $ ExitFailure 1))
    ]
