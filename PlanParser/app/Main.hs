{-# LANGUAGE GADTs, StandaloneDeriving, DataKinds, DeriveFunctor, DeriveTraversable #-}
{-# LANGUAGE KindSignatures, PolyKinds, RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import System.Environment
import Data.ConfigFile
import Data.Either.Utils
import Control.Monad

import Text.Show.Pretty as PP hiding (List, Value, Float)

import ParsePlanInterface
import Database.PgCuckoo.GPrint

import Database.PgCuckoo.GetTable

import LogicalAST as A
import LogicalASTPP as P

import PAtoRA as PR
import RewriteRA as RW
import RAtoInAST as RTI

import qualified Database.PgCuckoo.Lib as L

import GenGraphViz

-- data SpecificLabel = L Integer
--   deriving(Show, Eq)

data SQLType = SQLQuery
             | SQLTargetExpr
             | SQLExpr
  deriving(Show, Eq)

type Query = SQL SQLQuery
type Expr  = SQL SQLExpr

data SQL :: SQLType -> label -> * where
  QBlock :: { select :: [SQL SQLTargetExpr label]
            , sq :: Maybe (SQL SQLQuery label) } -> Query label
  CRef   :: { col :: String, row :: String } -> SQL SQLExpr label
  ETargetExpr :: { n :: String, exp :: Main.Expr label } -> SQL SQLTargetExpr label
  CConst :: { const :: String } -> SQL SQLExpr label
  Label  :: forall k label. { label :: label, arg :: SQL k label } -> SQL k label

t1 :: Query a
t1 = QBlock [ETargetExpr "col1" $ CRef "a" "b", ETargetExpr "col2" $ CConst "42"] (Just (QBlock [] Nothing))

-- Example transformation functions
labelCRef :: SQL k [Char] -> SQL k [Char]
labelCRef r@(CRef {col=col, row=row}) = Label "foo" r -- (CRef col row)
labelCRef e = e

trCRef :: SQL a b -> SQL a b
trCRef (CRef {col=col, row=row}) = CRef (col++"foo") row
trCRef c@(QBlock {select=select}) = c {select=map trCRef select}
trCRef i = i

deriving instance Show b => Show (SQL a b)
deriving instance Eq b => Eq (SQL a b)
-- deriving instance Functor (SQL a b)
-- deriving instance Foldable (SQL a b)
-- deriving instance Traversable (SQL a b)

-- access list elements safely
(!!) :: [a] -> Int -> Maybe a
(!!) lst idx = if idx >= length lst
                then Nothing
                else Just $ lst Prelude.!! idx

main :: IO ()
main = do
    cmdArgs <- getArgs
    let configFile =
            case cmdArgs Main.!! 0 of
                Just j -> j -- config.ini file
                Nothing -> error "please provide a config file"
    config <- readfile emptyCP configFile
    let cp = forceEither config
    let authStr = forceEither $ get cp "Main" "dbauth" :: String

    case cmdArgs of
        [_, file] -> do
            query <- readFile file
            (plan, consts) <- parsePlan authStr query

            -- putStrLn "##### Parsed Plan #####"
            -- putStrLn $ PP.ppShow plan

            tableDataR <- getTableData authStr
            raPlan <- PR.parse tableDataR plan consts
            putStrLn "##### RA Plan #####"
            putStrLn $ PP.ppShow raPlan

            putStrLn "##### Rewritten RA Plan #####"
            -- raPlan <- PR.fixVarP raPlan
            (raPlan', lg) <- RW.rewrite raPlan
            raPlan' <- PR.fixVarP raPlan'
            putStrLn $ PP.ppShow raPlan'
            putStrLn $ "################# LOG #################"
            mapM_ putStrLn lg
            putStrLn $ "################# / LOG #################"
            putStrLn $ P.prettyP raPlan'

            dot1 <- gen raPlan
            dot <- gen raPlan'
            writeFile "in.dot" dot1
            writeFile "out.dot" dot
            -- Generate Postgres Plan again
            {-
            cuckooInAST <- RTI.rewrite raPlan'
            putStrLn $ PP.ppShow cuckooInAST

            inf <- L.checkAndGenerateStmt authStr cuckooInAST
            let (p, explain, execute) = inf
            writeFile "explain.sql" explain
            writeFile "execute.sql" execute
            -}
