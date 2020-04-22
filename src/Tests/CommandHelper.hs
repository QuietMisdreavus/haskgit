module Tests.CommandHelper
    ( testRepo
    , mkTmpPath
    , writeTestFile
    , makeTestFileExecutable
    , testRunCommand
    , runTest
    ) where

import Data.Bitraversable
import Control.Exception
import qualified Data.Map.Strict as Map
import System.Directory
import System.Exit (ExitCode)
import System.FilePath
import System.IO
import System.Posix.IO

import Command
import Command.Base
import Repository

mkPipe :: IO (Handle, Handle)
mkPipe = createPipe >>= bitraverse fdToHandle fdToHandle

mkTmpPath :: IO String
mkTmpPath = canonicalizePath "test-repo"

testRepo :: IO Repository
testRepo = mkRepository <$> (</> ".git") <$> mkTmpPath

writeTestFile :: String -> String -> IO ()
writeTestFile name contents = do
    fullPath <- (</> name) <$> mkTmpPath
    createDirectoryIfMissing True $ takeDirectory fullPath
    writeFile fullPath contents

makeTestFileExecutable :: String -> IO ()
makeTestFileExecutable name = do
    fullPath <- (</> name) <$> mkTmpPath
    setPermissions fullPath
        $ setOwnerExecutable True
        $ setOwnerReadable True
        $ setOwnerWritable True
        $ emptyPermissions

testRunCommand :: [String] -> IO ExitCode
testRunCommand argv = do
    (fakeStdin, _) <- mkPipe
    (_, fakeStdout) <- mkPipe
    (fakeStderr, _) <- mkPipe
    repoDir <- mkTmpPath
    let env = CommandBase
            { commDir = repoDir
            , commEnv = Map.empty
            , commArgs = argv
            , commStdin  = fakeStdin
            , commStdout = fakeStdout
            , commStderr = fakeStderr
            }
    runCommand env

runTest :: IO a -> IO a
runTest action = do
    repoPath <- mkTmpPath
    testRunCommand ["init", repoPath]
    finally action $ removePathForcibly repoPath
