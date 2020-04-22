module Main where

import System.Exit
import Test.HUnit

import Tests.Index

allTests :: Test
allTests = TestList
    [ TestLabel "Index Tests" indexTests
    ]

main = do
    c <- runTestTT allTests
    if failures c > 0
        then exitFailure
        else exitSuccess
