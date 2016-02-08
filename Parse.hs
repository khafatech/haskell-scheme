module Parse where

import Control.Monad
-- TODO: could this be exporeted from Errors.hs?
import Control.Monad.Error (throwError)
import Numeric
import Data.Char

import Text.ParserCombinators.Parsec hiding (spaces)

-- local
import Types
import Errors


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


---- String parsing ----
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

binDigit :: Parser Char
binDigit = oneOf "01"

getBin :: String -> Integer
getBin numStr = let numList = map binChToNum numStr
  -- at each point we're multiplying the current digit by 2 and adding to rest.
  in foldl (\x y -> 2 * x + y) 0 numList

parseNumberBase parseFunc readFunc = liftM (Number . readFunc) $ many1 parseFunc

-- different ways of writing parseDecNumber.
-- parseNumber =  (many1 digit)  >>= (\n -> return ((Number . read) n))
parseDecNumber :: Parser LispVal
parseDecNumber = parseNumberBase digit read


parseNumber :: Parser LispVal
parseNumber = do
                char '#'
                base <- oneOf "bBoOdDxX"
                case (toLower base) of
                    'b' -> parseNumberBase binDigit getBin
                    'o' -> parseNumberBase octDigit getOct
                    'd' -> parseDecNumber
                    'x' -> parseNumberBase hexDigit getHex




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




