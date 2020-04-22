module Tests.Command.Add
    ( addCommandTests
    ) where

import Control.Arrow ((&&&))
import Data.Word
import Test.HUnit

import Index
import Index.Entry
import Repository
import Tests.CommandHelper

addCommandTests :: Test
addCommandTests = TestList
    [ TestLabel "adds a regular file to the index" addSingleFile
    , TestLabel "adds an executable file to the index" addExecutableFile
    , TestLabel "adds multiple files to the index" addMultipleFiles
    , TestLabel "incrementally adds files to the index" incrementallyAddFiles
    , TestLabel "adds a directory to the index" addDirectory
    , TestLabel "adds the repository root to the index" addRepoRoot
    , TestLabel "is silent on success" silentOnSuccess
    , TestLabel "fails for nonexistent files" nonexistentFailure
    , TestLabel "fails for unreadable files" unreadableFailure
    , TestLabel "fails if the index is locked" indexLockedFailure
    ]

assertIndex :: [(Word32, String)] -> Assertion
assertIndex expected = do
    (idx, _) <- getRepoReadIndex =<< testRepo
    let actual = map (entryMode &&& entryPath) $ indexEntries idx
    assertEqual "" expected actual

addSingleFile :: Test
addSingleFile = TestCase $ runTest $ do
    writeTestFile "hello.txt" "hello"
    testRunCommand ["add", "hello.txt"]
    assertIndex [(0o100644, "hello.txt")]

addExecutableFile :: Test
addExecutableFile = TestCase $ runTest $ do
    writeTestFile "hello.txt" "hello"
    makeTestFileExecutable "hello.txt"
    testRunCommand ["add", "hello.txt"]
    assertIndex [(0o100755, "hello.txt")]

addMultipleFiles :: Test
addMultipleFiles = TestCase $ runTest $ do
    writeTestFile "hello.txt" "hello"
    writeTestFile "world.txt" "world"
    testRunCommand ["add", "hello.txt", "world.txt"]
    assertIndex [(0o100644, "hello.txt"), (0o100644, "world.txt")]

incrementallyAddFiles :: Test
incrementallyAddFiles = TestCase $ runTest $ do
    writeTestFile "hello.txt" "hello"
    writeTestFile "world.txt" "world"
    testRunCommand ["add", "world.txt"]
    assertIndex [(0o100644, "world.txt")]
    testRunCommand ["add", "hello.txt"]
    assertIndex [(0o100644, "hello.txt"), (0o100644, "world.txt")]

addDirectory :: Test
addDirectory = TestCase $ runTest $ do
    writeTestFile "a-dir/nested.txt" "content"
    testRunCommand ["add", "a-dir"]
    assertIndex [(0o100644, "a-dir/nested.txt")]

addRepoRoot :: Test
addRepoRoot = TestCase $ runTest $ do
    writeTestFile "a/b/c/file.txt" "content"
    testRunCommand ["add", "."]
    assertIndex [(0o100644, "a/b/c/file.txt")]

silentOnSuccess :: Test
silentOnSuccess = TestCase $ runTest $ do
    writeTestFile "hello.txt" "hello"
    out <- testRunCommand ["add", "hello.txt"]
    assertCommandStatus 0 out
    assertCommandStdout "" out
    assertCommandStderr "" out

nonexistentFailure :: Test
nonexistentFailure = TestCase $ runTest $ do
    out <- testRunCommand ["add", "no-such-file"]
    assertCommandStderr
        "fatal: pathspec 'no-such-file' did not match any files\n"
        out
    assertCommandStatus 128 out
    assertIndex []

unreadableFailure :: Test
unreadableFailure = TestCase $ runTest $ do
    writeTestFile "secret.txt" ""
    makeTestFileUnreadable "secret.txt"
    out <- testRunCommand ["add", "secret.txt"]
    assertCommandStderr
        "error: open('secret.txt'): Permission denied\n\
            \fatal: adding files failed\n"
        out
    assertCommandStatus 128 out
    assertIndex []

indexLockedFailure :: Test
indexLockedFailure = TestCase $ runTest $ do
    writeTestFile "file.txt" ""
    writeTestFile ".git/index.lock" ""
    out <- testRunCommand ["add", "file.txt"]
    assertCommandStatus 128 out
    assertIndex []
