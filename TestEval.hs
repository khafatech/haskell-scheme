module TestEval where

import Test.Tasty
import Test.Tasty.QuickCheck

import Types
import Errors
import Eval



-- eval (List (Atom func : args)) = mapM eval args >>= apply func
prop_eval_add:: Integer -> Integer -> Bool
prop_eval_add a b = case eval (List [Atom "+", Number a, Number b]) of
        Right (Number c) -> a + b == c
        _ -> False

-- FIXME - current implementation (2-11-16) only supports 2 or more args for
-- bin ops.  r6rs, however, supports 0 or more args for + and *.  zero args
-- return the identify funcs: (+) returns 0, (*) returns 1
prop_eval_add_wrongargs :: Integer -> Bool
prop_eval_add_wrongargs a = case eval (List [Atom "+", Number a]) of
        Left (NumArgs 2 _) -> True
        _ -> False

prop_eval_add1 :: Integer -> Bool
prop_eval_add1 a = case eval (List [Atom "+", Number a]) of
        Right (Number b) -> a == b
        _ -> False

prop_eval_add0 :: Bool
prop_eval_add0 = case eval (List [Atom "+"]) of
        Right (Number 0) -> True
        _ -> False



exprList :: String -> [LispVal] -> LispVal
exprList h rest = List (Atom h : rest)

list :: [Integer] -> LispVal
list = List . (map Number)

--
-- expr1 = List [Atom "+", Number a, List [Atom]]
-- Here there's LispVal lists, lispVals, and haskell lists
-- we shouldn't mix them.
expr1 = List [Atom "+", Number 1, Number 3, Number 5]
expr2 = List [Atom "+", List [Atom "-", Number 11, Number 5],
                        List [Atom "*", Number 3, Number 4]]

expr3 a b c d = exprList "+" [
                       exprList "-" [Number a, Number b]
                     , exprList "*" [Number c, Number d]
                     ]
                                    
prop_eval_nested_expr :: Integer -> Integer -> Integer -> Integer -> Bool
prop_eval_nested_expr a b c d = case eval (expr3 a b c d) of
        Right (Number res) -> res == ((a - b) + (c * d))
        _ -> False

testEval = testGroup "Eval"
    [
        testProperty "add two ints" prop_eval_add
    ,   testProperty "FIXME add one int (wrong args)" prop_eval_add_wrongargs
    ,   testProperty "add one int" prop_eval_add1
    ,   testProperty "add zero ints" prop_eval_add0
    ,   testProperty "Nested expr" prop_eval_nested_expr

    ]


