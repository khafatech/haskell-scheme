module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Char



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


readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val


eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args


evalStr :: String -> LispVal
evalStr = eval . readExpr

-- lookup is a builtin
-- apply :: String -> [LispVal] -> LispVal
-- apply func args = maybe (Bool False) ($ args) $ lookup func primitives
apply func args = case lookup func primitives of
            Just f -> f args
            Nothing -> Bool False
                
-- maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $  map unpackNum params

-- not doing weak typing
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0


main :: IO ()
main = do args <- getArgs
          putStrLn . show . eval . readExpr . (!! 0) $ args
