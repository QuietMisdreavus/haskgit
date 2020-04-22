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
    , TestLabel "incrementally adds files to the index" incrementallyAddFiles
    , TestLabel "adds a directory to the index" addDirectory
    , TestLabel "adds the repository root to the index" addRepoRoot
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
