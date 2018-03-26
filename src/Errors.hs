module Errors ( LispError(..)
              , ThrowsError
              , throwError
              , trapError
              , extractValue
              ) where

import Control.Monad.Except (throwError, catchError, MonadError)
import Text.ParserCombinators.Parsec
import Types


-- for an example of how to use a custom error data type, see
-- See https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html#g:3

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
showError (Default err)             = "Error: " ++ err


instance Show LispError where show = showError

{-
 - --this isn't needed
instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default
-}

-- Currying the Either type constructor.
-- "Either a b" means Left a, Right b
type ThrowsError = Either LispError


-- Now "ThrowsError a", a is the value. The error LispError is embedded in ThrowsError
extractValue :: ThrowsError a -> a
extractValue (Right val) = val


-- this is like an error handler.
-- action is a result of a monadic computation (e.g. do notation.)
-- What handler does is show the value and wrap it the monad
-- catchError :: m a -> (e -> m a) -> m a
trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)


