module Main (main) where

import System.Exit
import Test.HUnit

import Tests.Index
import Tests.Command

allTests :: Test
allTests = TestList
    [ TestLabel "Index Tests" indexTests
    , TestLabel "Command Tests" commandTests
    ]

main = do
    c <- runTestTT allTests
    if failures c > 0
        then exitFailure
        else exitSuccess
