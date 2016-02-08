module Types where

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

