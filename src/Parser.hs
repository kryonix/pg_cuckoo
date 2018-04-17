
module Parser(parseStr) where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Expr = Join { foo :: Atom }
    deriving(Show)

data Atom = Str String
          | Int Integer
          | Var String
          | E   Expr
    deriving (Show)

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | 'integer' parses an integer.
integer :: Parser Integer
integer = lexeme L.decimal

-- | 'semi' parses a semicolon.
semi :: Parser String
semi = symbol ";"


rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["join"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x


parseStr :: String -> Expr
parseStr str =
    case parse whileParser "" str of
        Left e  -> error $ parseErrorPretty' str e
        Right r -> r

whileParser :: Parser Expr
whileParser = between sc eof stmt

stmt :: Parser Expr
stmt = f <$> sepBy1 stmt' semi
  where
    -- if there's only one stmt return it without using ‘Seq’
    f l = head l -- if length l == 1 then head l else Seq l

stmt' :: Parser Expr
stmt' = joinStmt

joinStmt :: Parser Expr
joinStmt = do
    rword "join"
    foo <- parens fooStmt
    return $ Join foo

fooStmt :: Parser Atom
fooStmt = Str   <$> stringLiteral
        <|> Int <$> integer
        <|> Var <$> identifier
        <|> E   <$> stmt'

stringLiteral :: Parser String
stringLiteral = lexeme (char '"' >> manyTill L.charLiteral (char '"'))