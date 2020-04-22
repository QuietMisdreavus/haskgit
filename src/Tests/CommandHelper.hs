module Tests.CommandHelper
    ( testRepo
    , mkTmpPath
    , writeTestFile
    , makeTestFileExecutable
    , makeTestFileUnreadable
    , CommandOutput(..)
    , testRunCommand
    , runTest
    , assertCommandStatus
    , assertCommandStdout
    , assertCommandStderr
    ) where

import Data.Bitraversable
import Control.Exception
import qualified Data.Map.Strict as Map
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO
import System.Posix.IO
import Test.HUnit

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

makeTestFileUnreadable :: String -> IO ()
makeTestFileUnreadable name = do
    fullPath <- (</> name) <$> mkTmpPath
    setPermissions fullPath emptyPermissions

data CommandOutput = CommandOutput
    { outputCode :: ExitCode
    , outputStdout :: Handle
    , outputStderr :: Handle
    }

testRunCommand :: [String] -> IO CommandOutput
testRunCommand argv = do
    (fakeStdin, _) <- mkPipe
    (outputStdout, fakeStdout) <- mkPipe
    (outputStderr, fakeStderr) <- mkPipe
    repoDir <- mkTmpPath
    let env = CommandBase
            { commDir = repoDir
            , commEnv = Map.empty
            , commArgs = argv
            , commStdin  = fakeStdin
            , commStdout = fakeStdout
            , commStderr = fakeStderr
            }
    code <- runCommand env
    hClose fakeStdin
    hClose fakeStdout
    hClose fakeStderr
    return $ CommandOutput code outputStdout outputStderr

runTest :: IO a -> IO a
runTest action = do
    repoPath <- mkTmpPath
    testRunCommand ["init", repoPath]
    finally action $ removePathForcibly repoPath

assertCommandStatus :: Int -> CommandOutput -> Assertion
assertCommandStatus expected (CommandOutput { outputCode = code }) =
    if expected == 0
        then assertEqual "" ExitSuccess code
        else assertEqual "" (ExitFailure expected) code

assertCommandStdout :: String -> CommandOutput -> Assertion
assertCommandStdout expected (CommandOutput { outputStdout = h }) = do
    actual <- hGetContents h
    assertEqual "" expected actual

assertCommandStderr :: String -> CommandOutput -> Assertion
assertCommandStderr expected (CommandOutput { outputStderr = h }) = do
    actual <- hGetContents h
    assertEqual "" expected actual
