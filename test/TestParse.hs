module TestParse where

import Test.Tasty
import Test.Tasty.HUnit

import Types
import Errors
import Parse


{-

to run tests in stack:


stack ghci --test, select Spec.hs, then run main
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
    , ("(* 100 200)", List [Atom "*", Number 100, Number 200])

    , ("(*   100  200 )", List [Atom "*", Number 100, Number 200])
    ]



singleAtoms = map (\atom -> ([atom], Atom [atom])) "!$%&|*+-/:<=>?@^_~#"

testAtoms = 
    [
      ("abc", Atom "abc")
    , ("a", Atom "a")
    , ("!", Atom "!")
    , ("$!##@", Atom "$!##@")
    , ("a++1", Atom "a++1")
    , ("open/file", Atom "open/file")
    ]


testExpectedExpr :: String -> LispVal -> Maybe String
testExpectedExpr exprStr expectedVal = case readExpr exprStr of
    Right val -> if val == expectedVal then
                    Nothing
                 else Just ("expecting: " ++ (show expectedVal)
                            ++ ", but got: " ++ (show val))
    Left e -> Just (show e)


runList expectedExprs = map (\(exprStr, val) -> testCase ("Parsing: " ++ exprStr) $
            case testExpectedExpr exprStr val of
                Nothing -> assertBool ("passed") True
                Just msg -> assertBool ("failed: " ++ msg) False
    ) expectedExprs




testParse = testGroup "Parse" $
    [   testCase "list parse" $
            -- readExpr "1" @?= Right (Number 1)
            assertBool "message" True
    
    ,   testGroup "parse lists" $ runList expectedExprs
    ,   testGroup "parse single atoms" $ runList singleAtoms
    ,   testGroup "parse atoms" $ runList testAtoms
    ] 