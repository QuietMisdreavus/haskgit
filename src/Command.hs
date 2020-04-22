module Command
    ( runCommand
    ) where

import Control.Exception
import System.Exit

import Command.Add
import Command.Base
import Command.Commit
import Command.Init

runCommand :: CommandBase -> IO ExitCode
runCommand env =
    case commArgs env of
        [] -> commandError ""
        (comm:xs) -> handle
            (\code -> return (code :: ExitCode))
            (do
                let newEnv = env { commArgs = xs }
                case comm of
                    "init" -> doInit newEnv
                    "add" -> doAdd newEnv
                    "commit" -> doCommit newEnv
                    _ -> commandError comm
                return ExitSuccess)

commandError :: String -> IO a
commandError comm = ioError $ userError $ "'" ++ comm ++ "' is not a haskgit command."
