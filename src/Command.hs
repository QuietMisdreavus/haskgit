module Command
    ( parseCommand
    ) where

import Data.Maybe

import Command.Add
import Command.Commit
import Command.Init

parseCommand :: [String] -> IO ()
parseCommand ("init":xs) = doInit $ listToMaybe xs
parseCommand ("add":xs) = doAdd xs
parseCommand ("commit":_) = doCommit
parseCommand (comm:_) = commandError comm
parseCommand [] = commandError ""

commandError :: String -> IO ()
commandError comm = ioError $ userError $ "'" ++ comm ++ "' is not a haskgit command."
