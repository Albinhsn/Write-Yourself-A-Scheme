module Main where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric 

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Character Char
             | String String
             | Float Float
             | Bool Bool

parseExpr :: Parser LispVal
parseExpr = parseAtom 
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

parseFloat :: Parser LispVal
parseFloat = do
          x<- many1 digit
          char '.'
          y <- many1 digit
          return $ Float (fst.head$readFloat(x ++ "." ++ y))

parseAtom :: Parser LispVal
parseAtom = do 
            first <- letter <|> symbol
            rest <- many (letter <|> digit <|> symbol)
            let atom = first:rest
            return $ case atom of 
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _  -> Atom atom

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr 
  return $ List [Atom "quote", x]

parseCharacter :: Parser LispVal
parseCharacter = do 
  try $ string "#\\"
  value <- try (string "newline" <|> string "space") <|> 
    do {
      x <- anyChar; 
      notFollowedBy alphaNum ; 
      return $ case x of 
        '\t' -> "\t"
        '\n' -> "\n"
        '\\' -> "\\"
        otherwise -> [x]
        }
  return $ Character $ case value of 
    "space" -> ' '
    "newline" -> '\n'
    otherwise -> (value!!0)

parseNumber :: Parser LispVal
parseNumber = do 
                x <- many1 digit
                (return . Number . read) x

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
