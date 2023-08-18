module Main where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseExpr :: Parser LispVal
parseExpr = parseAtom 
          <|> parseString
          <|> parseNumber

parseAtom :: Parser LispVal
parseAtom = do 
            first <- letter <|> symbol
            rest <- many (letter <|> digit <|> symbol)
            let atom = first:rest
            return $ case atom of 
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _  -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseString :: Parser LispVal
parseString = do 
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String 
readExpr input = case parse parseExpr "lisp" input of 
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

main :: IO ()
main =  do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
