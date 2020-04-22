module Tests.Command
    ( commandTests
    ) where

import Test.HUnit

import Tests.Command.Add
import Tests.Command.Status

commandTests :: Test
commandTests = TestList
    [ TestLabel "add command tests" addCommandTests
    , TestLabel "status command tests" statusCommandTests
    ]
