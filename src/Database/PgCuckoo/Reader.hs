{-|
Module      : Reader
Description : Parses the PostgreSQL Log-Format
Copyright   : © Denis Hirn <denis.hirn@uni-tuebingen.de>
License     : AllRightsReserved
Maintainer  : Denis Hirn
-}

{-# LANGUAGE PatternSynonyms #-}

module Database.PgCuckoo.Reader
    ( parseLog
    , Chunk(..)
    , pattern Chunk
    , Fields
    , Value(..)
    ) where

import           Prelude hiding (sequence)
import           Text.Megaparsec
import           Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L
import           Control.Applicative as A hiding (many)
import           Control.Monad (void)
import           Data.Void
import           Data.Map as M
import           Data.List (intercalate)

import qualified Data.List.NonEmpty as E

-- The postgres internal function stringToNode is ordering-sensitive.
-- That's why we need to preserve the order of fields.
-- On the other hand we want to keep the Data.Map data-type.
-- That's not compatible, so we add the orderedFields-field to Chunk and
-- rename the constructor to ChunkOrdered.
-- Because we now don't want to rewrite all the other Modules depending on
-- Chunk with two fields, we define the PatternSynonym Chunk, which matches
-- the old data-type-definition. This way we can dump the Log back to PG-Notation
-- preserving the order and utilize Data.Map.

pattern Chunk _chunkName _fields <- ChunkOrdering { _chunkName=_chunkName
                                                 , _fields=_fields
                                                 }

data Chunk = ChunkOrdering {_chunkName :: String, _fields :: Fields, _orderedFields :: [(String, Value)]}
    -- deriving (Show)

type Fields = Map String Value

data Value = NoValue
           | Int Integer
           | Bool Bool
           | Str String
           | Dbl Double
           | VChunk Chunk
           | Sequence [Integer]
           | List [Value]
           | Extra Value Value
    -- deriving (Show)

type Parser = Parsec Void String

instance Show Chunk where
    show (ChunkOrdering name _ fields) = "{" ++ name ++ " " ++ intercalate " " flds ++ "}"
        where
            flds = [":" ++ k ++ " " ++ show v | (k, v) <- fields]
            -- flds = [":" ++ k ++ " " ++ show v | (k, v) <- M.toList fields]

instance Show Value where
    show NoValue = "<>"
    show (Int i) = show i
    show (Dbl d) = show d
    show (Bool True) = "true"
    show (Bool False) = "false"
    show (Str s) = s
    show (VChunk c) = show c
    show (Sequence c) = "[ " ++ lst ++ " ]"
        where
            lst = intercalate " " $ Prelude.map show c
    show (List l) = "(" ++ lst ++ ")"
        where
            lst = intercalate " " $ Prelude.map show l
    show (Extra v1 v2) = show v1 ++ " " ++ show v2

parseLog :: String -> [Chunk]
parseLog str =
    case parse program "" str of
        -- Left e  -> error $ parseErrorPretty' (Prelude.drop ((unPos $ sourceColumn $ E.head $ errorPos e)-30) str) e
        Left e  -> error $ parseErrorPretty' str e
        Right r -> r


program :: Parser [Chunk]
program = sc *> (many chunk) <* eof


-- space consumer
sc :: Parser ()
sc = L.space (void spaceChar) A.empty A.empty

chunk :: Parser Chunk
chunk = do
    res <- tCurls $ do
                        s <- identifier
                        f <- many tag
                        let f' = fromList f :: Fields
                        return $ ChunkOrdering s f' f
    return res


tag :: Parser (String, Value)
tag = do
    _ <- tColon
    -- below: parse until first space; thereafter, remove all whitespaces
    s <- (manyTill L.charLiteral spaceChar) <* sc
    v <- value <|> (return NoValue)
    return $ (s, v)


value :: Parser Value
value = do
    res <- (try dblV
        <|> integerV)
        <|> boolean
        <|> str
        <|> unit
        <|> chunkVal
        <|> list
        <|> sequence
        <|> identifierVal
    opt <- optional sequence
    res' <- case opt of
            Nothing -> return res
            Just x  -> return $ Extra res x
    return res'


identifierVal :: Parser Value
identifierVal = do
    s <- identifier
    return $ Str s


boolean :: Parser Value
boolean = (tTrue >> return (Bool True))
      <|> (tFalse >> return (Bool False))


str :: Parser Value
str = do
    res <- tQuotes $ do
                        f <- many ((alphaNumChar 
                                    <|> qMark 
                                    <|> M.char '=' 
                                    <|> M.char '+' 
                                    <|> M.char '\\'
                                    <|> M.char '<'
                                    <|> M.char '_'
                                    <|> M.char '('
                                    <|> M.char ')'
                                    <|> M.char '['
                                    <|> M.char ']'
                                    <|> M.char '.'
                                    <|> M.char '$') <* M.space)
                        return f
    return $ Str res

dblV :: Parser Value
dblV = do
    res <- lexeme L.float
    return $ Dbl res

integerV :: Parser Value
integerV = do
    res <- integer
    return $ Int res


chunkVal :: Parser Value
chunkVal = do
    res <- chunk
    return $ VChunk res


list :: Parser Value
list = do
    res <- tParens $ many value
    return $ List res


sequence :: Parser Value
sequence = do
    res <- tBracks $ many integer
    return $ Sequence res


unit :: Parser Value
unit = do
    _ <- tUnit
    return NoValue

--------------------------------------------------------------------------------
-- literals

identifier :: Parser String
identifier = lexeme p
    where
        p = (:) <$> (underscore <|> sp <|> star <|> letterChar <|> qMark) 
                    <*> many ((underscore <|> sp <|> star <|> qMark <|> alphaNumChar) <* M.space)


integer :: Parser Integer
integer = do
    sig <- optional $ symbol "-"
    i <- lexeme L.decimal
    let res = case sig of
                Nothing -> i
                Just _  -> -i
    return res

star :: Parser Char
star = M.char '*'

sp :: Parser Char
sp = M.char '\\' *> M.char ' '

underscore :: Parser Char
underscore = M.char '_'

qMark :: Parser Char
qMark = M.char '?'

tUnit :: Parser String
tUnit = symbol "<>"

tTrue :: Parser String
tTrue = symbol "true"

tFalse :: Parser String
tFalse = symbol "false"

tsemi :: Parser String
tsemi = symbol ";"

tColon :: Parser String
tColon = symbol ":"

tCurls :: Parser a -> Parser a
tCurls = between (symbol "{") (symbol "}")

tParens :: Parser a -> Parser a
tParens = between (symbol "(") (symbol ")")

tBracks :: Parser a -> Parser a
tBracks = between (symbol "[") (symbol "]")

tQuotes :: Parser a -> Parser a
tQuotes = between (symbol "\"") (symbol "\"")

--------------------------------------------------------------------------------
-- other helpers

-- eat a symbol; remove whitespaces at the end
symbol :: String -> Parser String
symbol = L.symbol sc


-- eat a pattern; remove whitespaces at the end
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
