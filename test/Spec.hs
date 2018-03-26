module Main where

import Test.Tasty

import TestEval
import TestParse

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [testEval, testParse]
