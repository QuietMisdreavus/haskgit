module Tests.Command
    ( commandTests
    ) where

import Test.HUnit

import Tests.Command.Add

commandTests :: Test
commandTests = TestList
    [ TestLabel "add command tests" addCommandTests
    ]
