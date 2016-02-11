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

testEval = testGroup "Eval"
    [
        testProperty "add two ints" prop_eval_add
    ,   testProperty "FIXME add one int (wrong args)" prop_eval_add_wrongargs
    ,   testProperty "add one int" prop_eval_add1
    ,   testProperty "add zero ints" prop_eval_add0

    ]


