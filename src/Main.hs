module Main (main) where

import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

data Type = IntegerType | StringType | CustomType String [(String, Type)] deriving (Show)

data Expr
  = IntLiteral Integer
  | StringLiteral String
  | TypeDecl String [(String, Type)] -- for custom types
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

field :: Parser (String, Type)
field = do
  fieldName <- Tok.identifier lexer
  fType <- varType
  return (fieldName, fType)

typeDecl :: Parser Expr
typeDecl = do
  _ <- Tok.symbol lexer "data"
  typeName <- Tok.identifier lexer
  _ <- Tok.symbol lexer "="
  fields <- Tok.braces lexer (field `sepBy` (Tok.symbol lexer ","))
  return $ TypeDecl typeName fields

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
generateTypeScript (TypeDecl name fields) =
  "interface " ++ name ++ " {\n" ++ concatMap generateField fields ++ "}"
  where
    generateField (fname, ftype) = "  " ++ fname ++ ": " ++ generateTypeScriptType ftype ++ ";\n"
generateTypeScript (VarDecl name vtype value) =
  "let " ++ name ++ ": " ++ generateTypeScriptType vtype ++ " = " ++ generateValue value ++ ";"
  where
    generateValue (IntLiteral i) = show i
    generateValue (StringLiteral s) = show s

generateTypeScriptType :: Type -> String
generateTypeScriptType IntegerType = "number"
generateTypeScriptType StringType = "string"
generateTypeScriptType (CustomType name _) = name

main :: IO ()
main = do
  let intTest = VarDecl "x" IntegerType (IntLiteral 42)

  -- This should produce "interface User { name: string; age: number; }"
  let userTest = TypeDecl "User" [("name", StringType), ("age", IntegerType)]

  -- This should produce "let y: string = \"Hello\";"
  let strTest = VarDecl "y" StringType (StringLiteral "Hello")

  let ast = map generateTypeScript [intTest, userTest, strTest]
  writeFile "output.ts" $ unlines ast
