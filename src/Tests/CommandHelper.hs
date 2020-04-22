module Tests.CommandHelper
    ( testRepo
    , mkTmpPath
    , writeTestFile
    , makeTestFileExecutable
    , makeTestFileUnreadable
    , CommandOutput(..)
    , testRunCommand
    , testCommit
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

mkTmpPath :: IO FilePath
mkTmpPath = canonicalizePath "test-repo"

testRepo :: IO Repository
testRepo = mkRepository . (</> ".git") <$> mkTmpPath

writeTestFile :: FilePath -> String -> IO ()
writeTestFile name contents = do
    fullPath <- (</> name) <$> mkTmpPath
    createDirectoryIfMissing True $ takeDirectory fullPath
    writeFile fullPath contents

makeTestFileExecutable :: FilePath -> IO ()
makeTestFileExecutable name = do
    fullPath <- (</> name) <$> mkTmpPath
    setPermissions fullPath
        $ setOwnerExecutable True
        $ setOwnerReadable True
        $ setOwnerWritable True emptyPermissions

makeTestFileUnreadable :: FilePath -> IO ()
makeTestFileUnreadable name = do
    fullPath <- (</> name) <$> mkTmpPath
    setPermissions fullPath emptyPermissions

data CommandOutput = CommandOutput
    { outputCode :: ExitCode
    , outputStdout :: Handle
    , outputStderr :: Handle
    }

testRunCommand :: [String] -> IO CommandOutput
testRunCommand = testRunCommandFull "" Map.empty

testRunCommandFull :: String -> Map.Map String String -> [String] -> IO CommandOutput
testRunCommandFull input envVars argv = do
    (fakeStdin, setStdin) <- mkPipe
    (outStdout, fakeStdout) <- mkPipe
    (outStderr, fakeStderr) <- mkPipe
    repoDir <- mkTmpPath
    if not $ null input
        then hPutStrLn setStdin input
        else pure ()
    hClose setStdin
    let env = CommandBase
            { commDir = repoDir
            , commEnv = envVars
            , commArgs = argv
            , commStdin  = fakeStdin
            , commStdout = fakeStdout
            , commStderr = fakeStderr
            }
    code <- runCommand env
    hClose fakeStdin
    hClose fakeStdout
    hClose fakeStderr
    return $ CommandOutput code outStdout outStderr

testCommit :: String -> IO CommandOutput
testCommit msg = do
    let envVars = Map.fromList
            [ ("GIT_AUTHOR_NAME", "A. U. Thor")
            , ("GIT_AUTHOR_EMAIL", "author@example.com")
            ]
    testRunCommandFull msg envVars ["commit"]

runTest :: IO a -> IO a
runTest action = do
    repoPath <- mkTmpPath
    _ <- testRunCommand ["init", repoPath]
    finally action $ removePathForcibly repoPath

assertCommandStatus :: Int -> CommandOutput -> Assertion
assertCommandStatus expected CommandOutput { outputCode = code } =
    if expected == 0
        then assertEqual "" ExitSuccess code
        else assertEqual "" (ExitFailure expected) code

assertCommandStdout :: String -> CommandOutput -> Assertion
assertCommandStdout expected CommandOutput { outputStdout = h } = do
    actual <- hGetContents h
    assertEqual "" expected actual

assertCommandStderr :: String -> CommandOutput -> Assertion
assertCommandStderr expected CommandOutput { outputStderr = h } = do
    actual <- hGetContents h
    assertEqual "" expected actual
