module Eval where

import Types
import Errors

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
-- TODO - handle function application?
eval val@(Atom _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, thenp, elsep]) = do
        case (eval pred) of
            Right (Bool False) -> eval elsep
            Right _ -> eval thenp
            Left err -> throwError $ err


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
primitives = [("+", numericBinopZeroArg (+) 0),
              ("*", numericBinopZeroArg (*) 1),

              -- FIXME: these should work with 1 arg or more.
              ("-", numericBinop (-)),
              ("/", numericBinop div),

              ("^", numericBinop (^)),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),

              -- FIXME in r5rs, these work with two or more args
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),

              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),

              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),

              -- TODO: separate these into another list
              ("number?", testType "number" . head),
              ("string?", testType "string" . head),
              ("symbol?", testType "symbol" . head),

              ("eq?", eqv),
              ("eqv?", eqv),

              ("car", car),
              ("cdr", cdr),
              ("cons", cons)

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


-- in rnrs, racket and clojure, * and + can accept 0 and 1 args
--      (-) and / can accept 1 arg. e.g. (- 4) == -4
numericBinopZeroArg :: (Integer -> Integer -> Integer) -> Integer -> [LispVal] -> ThrowsError LispVal
numericBinopZeroArg _ base [] = return $ Number base
numericBinopZeroArg op base params = mapM unpackNum params >>= return . Number . foldl op base

numericAdd = numericBinopZeroArg (+) 0
numericMul = numericBinopZeroArg (*) 1


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ oneVal@[_] = throwError $ NumArgs 2 oneVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op
-- non-monadic:
-- numericBinop op params = Number $ foldl1 op $  map unpackNum params


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op [leftVal, rightVal] = do
                        left <- unpacker leftVal
                        right <- unpacker rightVal
                        return $ Bool $ left `op` right

boolBinop unpacker op args = throwError $ NumArgs 2 args

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notStr = throwError $ TypeMismatch "string" notStr

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool


-- car, cdr, and cons -----------------------
--
-- car takes in a list of LispVals since car can syntactically take multiple
-- arguments. e.g. if it was called with
-- (car '(1 2) '(3 4)). In this case, it would throw an error.
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList


cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList


cons :: [LispVal] -> ThrowsError LispVal
-- (cons a nil) => (a)
cons [x1, List []] = return $ List [x1]
-- (cons a list) => (a)
cons [x, List xs] = return $ List (x:xs)

-- DottedList should stay DottedList
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast

-- consing two non-lists makes a DottedList
cons [x1, x2] = return $ DottedList [x1] x2

cons badArgList = throwError $ NumArgs 2 badArgList

-- eqv ----

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List (xs ++ [x]), List (ys ++ [y])]

-- tests if all elements in the list are `eqv` and of the same length
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) &&
                                                             (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                            Left err -> False
                            Right (Bool val) -> val

eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList





