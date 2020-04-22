module Tests.Command.Status
    ( statusCommandTests
    ) where

import Test.HUnit

import Tests.CommandHelper

statusCommandTests :: Test
statusCommandTests = TestList
    [ TestLabel "lists untracked files in name order" untrackedFiles
    , TestLabel "lists files as untracked if they are not in the index" notIndexFiles
    , TestLabel "lists untracked directories, not their contents" untrackedDirs
    , TestLabel "lists untracked files inside tracked directories" untrackedFilesTrackedDirs
    ]

assertRepoStatus :: String -> Assertion
assertRepoStatus expected =
    testRunCommand ["status"] >>= assertCommandStdout expected

untrackedFiles :: Test
untrackedFiles = TestCase $ runTest $ do
    writeTestFile "file.txt" ""
    writeTestFile "another.txt" ""
    assertRepoStatus $ unlines
        [ "?? another.txt"
        , "?? file.txt"
        ]

notIndexFiles :: Test
notIndexFiles = TestCase $ runTest $ do
    writeTestFile "committed.txt" ""
    testRunCommand ["add", "."]
    testCommit "commit message"
    writeTestFile "file.txt" ""
    assertRepoStatus $ unlines
        [ "?? file.txt"
        ]

untrackedDirs :: Test
untrackedDirs = TestCase $ runTest $ do
    writeTestFile "file.txt" ""
    writeTestFile "dir/another.txt" ""
    assertRepoStatus $ unlines
        [ "?? dir/"
        , "?? file.txt"
        ]

untrackedFilesTrackedDirs :: Test
untrackedFilesTrackedDirs = TestCase $ runTest $ do
    writeTestFile "a/b/inner.txt" ""
    testRunCommand ["add", "."]
    testCommit "commit message"
    writeTestFile "a/outer.txt" ""
    writeTestFile "a/b/c/file.txt" ""
    assertRepoStatus $ unlines
        [ "?? a/b/c/"
        , "?? a/outer.txt"
        ]
