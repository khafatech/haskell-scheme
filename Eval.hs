module Eval where

-- TODO: could this be exporeted from Errors.hs?
import Control.Monad.Error (throwError)

import Types
import Errors

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
-- TODO - handle function application?
eval val@(Atom _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badform = throwError $ BadSpecialForm "Unrecognized special form" badform



-- lookup is a builtin
-- apply :: String -> [LispVal] -> LispVal
-- apply func args = maybe (Bool False) ($ args) $ lookup func primitives
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = case lookup func primitives of
            Just f -> f args
            Nothing -> throwError $ NotFunction "Not a primitive function" func
                

-- maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("^", numericBinop (^)),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),

              -- TODO: separate these into another list
              ("number?", testType "number" . head),
              ("string?", testType "string" . head),
              ("symbol?", testType "symbol" . head)
              ]


testType :: String -> LispVal -> ThrowsError LispVal
testType "string" (String _) = return $ Bool True
testType "number" (Number _) = return $ Bool True
testType "symbol" (List [Atom "quote", _]) = return $ Bool True
testType _ _ = return $ Bool False


{-
numberq :: [LispVal] -> LispVal
numberq [Number _] = Bool True
numberq [_] = Bool False

stringq :: [LispVal] -> LispVal
stringq [Number _] = Bool True
stringq [_] = Bool False
-}


-- FIXME - in r5rs, racket and clojure, * and + can accept 0 and 1 args
--      (-) and / can accept 1 arg. e.g. (- 4) == -4
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op oneVal@[_] = throwError $ NumArgs 2 oneVal
-- non-monadic
-- numericBinop op params = Number $ foldl1 op $  map unpackNum params
-- monadic
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum


