module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad



data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool


-- this is better done with the 'show' type class
lispValtoStr :: LispVal -> String
lispValtoStr (Atom s) = s
lispValtoStr (String s) = s
lispValtoStr (Number n) = show n

showEither :: Either ParseError LispVal -> String
showEither (Left err) = "Error: " ++ show err
showEither (Right val) = lispValtoStr val


-- prints result of parse
-- e.g.: perr $ parse parseString "myparser" "\"abcdefg\\\\ikjlmn\"" 
perr either = putStrLn $ showEither either


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"


spaces :: Parser ()
spaces = skipMany1 space


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
           escapeCode <- (oneOf "\"nrt\\")
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


parseNumber :: Parser LispVal
parseNumber =  (many1 digit)  >>= (\n -> return ((Number . read) n))

--parseNumber = liftM (Number . read) $ many1 digit
{-
parseNumber = do
                num <- many1 digit
                return $ ((Number . read) num)
-}

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber



readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"


main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
