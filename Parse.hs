module Main where
import System.Environment
import System.IO

import Control.Monad
import Control.Monad.Error

import Numeric
import Data.Char

import Text.ParserCombinators.Parsec hiding (spaces)



data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool


-- this is better done with the 'show' type class
showVal :: LispVal -> String
showVal (Atom s) = s
showVal (String s) = "\"" ++ s ++ "\""
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- make instance of the Show type class
instance Show LispVal where show = showVal


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default


-- Currying the Either type constructor.
-- "Either a b" means Left a, Right b
type ThrowsError = Either LispError


-- Now "ThrowsError a", a is the value. The error LispError is embedded in ThrowsError
extractValue :: ThrowsError a -> a
extractValue (Right val) = val


-- this is like an error handler.
-- action is a result of a monadic computation (e.g. do notation.)
-- What handler does is show the value and wrap it the monad
trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)



-- End Error defs


showEither :: Either ParseError LispVal -> String
showEither (Left err) = "Error: " ++ show err
showEither (Right val) = showVal val


-- prints result of parse
-- e.g.: perr $ parse parseString "myparser" "\"abcdefg\\\\ikjlmn\"" 
perr either = putStrLn $ showEither either


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"


spaces :: Parser ()
spaces = skipMany1 space



---- String parsing
insideString :: Parser [Char]
insideString = do
                x <- many (noneOf "\"")
                return x


escape :: Char -> Char
escape 'n' = '\n'
escape 'r' = '\r'
escape 't' = '\t'
escape '\\' = '\\'
escape '"' = '"'
escape ch = error ("Unsupported escape character: '" ++ [ch] ++ "'")


slash :: Parser Char
slash = do
           char '\\'
           escapeCode <- oneOf "\"nrt\\"
           return $ escape escapeCode


parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (slash <|> (noneOf "\""))
                char '"'
                return $ String x



parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _    -> Atom atom

---- Parsing numbers
-- Each number base needs a function to parse its digits (e.g. hexDigit)
-- and a function to convert a string of digits to an integer

-- getIntBase ::  String -> Integer
-- This is for the read* functions in Numeric
getIntBase numReader str = case numReader str of
              [(n, "")] -> n
              [(_, extra)] -> error ("Unable to parse digits: " ++ extra)
              _ -> error ("Unable to parse number: " ++ str)

getOct :: String -> Integer
getOct = getIntBase readOct

getHex :: String -> Integer
getHex = getIntBase readHex



binChToNum :: Char -> Integer
binChToNum '1' = 1
binChToNum '0' = 0

getBin :: String -> Integer
getBin numStr = let numList = map binChToNum numStr
  -- at each point we're multiplying the current digit by 2 and adding to rest.
  in foldl (\x y -> 2 * x + y) 0 numList


parseNumberBase parseFunc readFunc = liftM (Number . readFunc) $ many1 parseFunc

parseDecNumber :: Parser LispVal
parseDecNumber = parseNumberBase digit read

binDigit :: Parser Char
binDigit = oneOf "01"


parseNumber :: Parser LispVal
parseNumber = do
                char '#'
                base <- oneOf "bBoOdDxX"
                case (toLower base) of
                    'b' -> parseNumberBase binDigit getBin
                    'o' -> parseNumberBase octDigit getOct
                    'd' -> parseDecNumber
                    'x' -> parseNumberBase hexDigit getHex


-- different ways of writing parseDecNumber.
-- parseNumber =  (many1 digit)  >>= (\n -> return ((Number . read) n))
--


-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
-- It takes a function a -> b, and a "Monad a", and applies a to 
-- the func and returns type "Monad b"

--parseNumber = liftM (Number . read) $ many1 digit

{-
parseNumber = do
                num <- many1 digit
                return $ ((Number . read) num)
-}




parseExpr :: Parser LispVal
--parseExpr = (many spaces) >> (parseNumber
parseExpr = parseNumber
         <|> parseDecNumber
         <|> parseString
         <|> parseAtom
         <|> parseQuoted
         <|> do char '('
                x <- (try parseList) <|> parseDottedList
                char ')'
                return x


parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

{- another way of doing it:
parseList = liftM List $ do
                 list <- sepBy parseExpr spaces
                 return list
-}


-- (a b c . d)
parseDottedList :: Parser LispVal
parseDottedList = do
                    head <- endBy parseExpr spaces
                    tail <- char '.' >> spaces >> parseExpr
                    return $ DottedList head tail


parseQuoted :: Parser LispVal
parseQuoted = do
                char '\''
                exp <- parseExpr
                return $ List [Atom "quote", exp]

showExpr :: String -> String
showExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found: " ++ show val


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lilscheme" input of
    -- Parser here is a LispError type constructor for a parsing error
    Left err -> throwError $ Parser err
    Right val -> return val


------------ eval ---------------

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


-- not doing weak typing
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
-- FIXME handle type errors (not a number)
unpackNum notNum = throwError $ TypeMismatch "number" notNum


-- evalStr :: String -> IO ()
evalStr input = do
    -- evaled <- return $ liftM show $ readExpr input >>= eval
    -- putStrLn $ extractValue $ trapError evaled
    readExpr input >>= eval


{-
main = do args <- getArgs
          putStrLn . show . eval . readExpr . (!! 0) $ args
-}

{-
-- eval stdin
main = do
    text <- hGetContents stdin
    print . eval . readExpr $ text
-}
-- or:
-- main :: IO ()
-- main = hGetContents stdin >>= print . eval . readExpr

main :: IO ()
main = do
    input <- hGetContents stdin
    -- this below executes within an IO (Either LispError String) action.
    evaled <- return $ liftM show $ readExpr input >>= eval
    putStrLn $ extractValue $ trapError evaled
