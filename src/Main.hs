module Main (main) where

import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

data Type = IntegerType deriving (Show)

data Expr
  = IntLiteral Integer
  | VarDecl String Type Expr
  deriving (Show)

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = []
    names = []
    style =
      emptyDef
        { Tok.reservedOpNames = ops,
          Tok.reservedNames = names,
          Tok.commentLine = "#"
        }

integer :: Parser Expr
integer = IntLiteral . fromIntegral <$> Tok.integer lexer

integerType :: Parser Type
integerType = Tok.symbol lexer ":: Integer" >> return IntegerType

varDecl :: Parser Expr
varDecl = do
  varName <- Tok.identifier lexer
  integerType
  value <- Tok.symbol lexer "=" >> integer
  return $ VarDecl varName IntegerType value

parseTestVar :: String -> Either ParseError Expr
parseTestVar input = parse varDecl "" input

main :: IO ()
main = print $ parseTestVar "x :: Integer = 5"
