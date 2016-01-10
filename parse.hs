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
--parseNumber = liftM (Number . read) $ many1 digit
{-
parseNumber = do
                num <- many1 digit
                return $ ((Number . read) num)
-}




parseExpr :: Parser LispVal
parseExpr = parseNumber
         <|> parseDecNumber
         <|> parseString
         <|> parseAtom


parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"


main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
