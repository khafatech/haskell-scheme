module Main where

import Test.Tasty

import TestEval


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [testEval]
