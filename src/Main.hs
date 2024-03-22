module Main (main) where

import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

data Type = IntegerType | StringType deriving (Show)

data Expr
  = IntLiteral Integer
  | StringLiteral String
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

stringLiteral :: Parser Expr
stringLiteral = StringLiteral <$> Tok.stringLiteral lexer

varType :: Parser Type
varType =
  (Tok.symbol lexer ":: Integer" >> return IntegerType)
    <|> (Tok.symbol lexer ":: String" >> return StringType)

varDecl :: Parser Expr
varDecl = do
  varName <- Tok.identifier lexer
  vType <- varType
  value <-
    Tok.symbol lexer "=" >> case vType of
      IntegerType -> integer
      StringType -> stringLiteral
  return $ VarDecl varName vType value

parseTestVar :: String -> Either ParseError Expr
parseTestVar input = parse varDecl "" input

generateTypeScript :: Expr -> String
generateTypeScript (IntLiteral i) = show i
generateTypeScript (StringLiteral s) = show s
generateTypeScript (VarDecl name IntegerType value) =
  "let " ++ name ++ ": number = " ++ generateTypeScript value ++ ";"
generateTypeScript (VarDecl name StringType value) =
  "let " ++ name ++ ": string = " ++ generateTypeScript value ++ ";"

main :: IO ()
main = do
  let intTest = VarDecl "x" IntegerType (IntLiteral 42)

  -- This should produce "let y: string = \"Hello\";"
  let strTest = VarDecl "y" StringType (StringLiteral "Hello")

  let ast = map generateTypeScript [intTest, strTest]
  writeFile "output.ts" $ unlines ast
