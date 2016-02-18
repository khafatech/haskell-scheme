module Main where
-- import System.Environment
import System.IO
import Control.Monad

import Types
import Errors
import Parse
import Eval



evalStr :: String -> ThrowsError LispVal
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
