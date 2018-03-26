module TestParse where

import Test.Tasty
import Test.Tasty.HUnit

import Types
import Errors
import Parse


{-

to run tests in stack:


stack ghci --test, select Spec.hs, then main
Type `:r` then `main` to run again on a reload

or:

# run all
stack test

# run only parse
stack test --ta '-p Parse' 

-}



expectedExprs :: [(String, LispVal)]
expectedExprs = 
    [ ("1", Number 1)
    , ("(+ 1 2)", List [Atom "+", Number 1, Number 2])
    , ("(+ 1 2 (- 5 3))", List [Atom "+", Number 1, Number 2,
                                          List [Atom "-", Number 5, Number 3]])

    ]


testExpectedExpr :: String -> LispVal -> Maybe String
testExpectedExpr exprStr expectedVal = case readExpr exprStr of
    Right val -> if val == expectedVal then
                    Nothing
                 else Just ("expecting: " ++ (show expectedVal)
                            ++ ", but got: " ++ (show val))
    Left e -> Just (show e)


tcs = map (\(exprStr, val) -> testCase ("Parsing: " ++ exprStr) $
            case testExpectedExpr exprStr val of
                Nothing -> assertBool ("passed") True
                Just msg -> assertBool ("failed: " ++ msg) False
    ) expectedExprs


testParse = testGroup "Parse" $
    [   testCase "list parse" $
            -- readExpr "1" @?= Right (Number 1)
            assertBool "message" True
    ] ++ tcs