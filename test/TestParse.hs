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



expectedSingleAtoms = conv "!$%&|*+-/:<=>?@^_~#"
    where
        conv = map (\atom -> ([atom], Atom [atom])) 


expectedAtoms = 
    [
      ("abc", Atom "abc")
    , ("a", Atom "a")
    , ("a b", Atom "a")
    , ("!", Atom "!")
    , ("$!##@", Atom "$!##@")
    , ("a++1", Atom "a++1")
    , ("open/file", Atom "open/file")
    , ("!set ", Atom "!set")
    , ("#c", Atom "#c")
    , ("Boo", Atom "Boo")
    , ("#t", Bool True)
    , ("#f", Bool False)
    ]

expectedVaues = 
    [
      ("\"hello world\"", String "hello world")
    , ("\"\"", String "")
    , ("\" \"", String " ")
    
    -- Stirngs with escape seq

    , ("\"\\n\"", String "\n")
    , ("\"\\r\"", String "\r")
    , ("\"\\t\"", String "\t")

    , ("(1 . 2)", DottedList [Number 1] (Number 2))
    , ("(1 2 . 3)", DottedList [Number 1, Number 2] (Number 3))
    , ("#xF", Number 15)
    ]


expectedNumbers =
    [
      ("5", 5)
    , ("001", 1)
    , ("010", 10)
    , ("-1", -1)
    , ("0", 0)
    , ("-1-2", -1)
    
    , ("1003", 1003)

    -- hex
    , ("#xff", 255)
    , ("#Xff", 255)
    , ("#x1f3ac5", 2046661)

    -- binary
    , ("#b101", 5)
    , ("#B101", 5)

    -- octal
    , ("#o10", 8)
    , ("#O10", 8)
    
    -- decimal
    , ("#d20", 20)
    , ("#D5102", 5102)
    ]

expectedNumbersLispVals = map (\(str, num) -> (str, Number num)) expectedNumbers


-- TODO - in the case of '-1-2', the first one could get parsed, and the 2nd minus is interpreted as the start of another number or operation

invalidNumbers =
    [
        "-1-2"
    ,   "1-0"
    ]




{-
This takes an expression as a string, the the expected value, and tries to parse the string.

If there was an error in parsing, or a mismatch, it returns the error message.

On success, it returns Nothing.
-}

testExpectedExpr :: String -> LispVal -> Maybe String
testExpectedExpr exprStr expectedVal = case readExpr exprStr of
    Right val -> if val == expectedVal then
                    Nothing
                 else Just ("expecting: " ++ (show expectedVal)
                            ++ ", but got: " ++ (show val))
    Left e -> Just (show e)



runList :: [([Char], LispVal)] -> [TestTree]
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
    ,   testGroup "parse single atoms" $ runList expectedSingleAtoms
    ,   testGroup "parse atoms" $ runList expectedAtoms

    ,   testGroup "parse the rest" $ runList expectedVaues
    ,   testGroup "parse numbers" $ runList expectedNumbersLispVals
    ] 
