{-|
Module      : Lib
Description : Cuckoo Interface
Copyright   : © Denis Hirn <denis.hirn@uni-tuebingen.de>
License     : AllRightsReserved
Maintainer  : Denis Hirn
-}

module Database.PgCuckoo.Lib
    ( parseConst
    , checkAndGenerateStmt
    ) where

import           Database.PgCuckoo.Reader    as R
import           Database.PgCuckoo.TrChunks  as T
import           Database.PgCuckoo.GetTable  as Td
import qualified Database.PgCuckoo.InAST     as I
import           Database.PgCuckoo.PgPlan    as O
import qualified Database.PgCuckoo.Validate  as V
import           Database.PgCuckoo.Inference as IF
import           Database.PgCuckoo.Extract   as E
import           Database.PgCuckoo.GPrint    as G

import Text.Show.Pretty as PP hiding (List, Value, Float)
import Debug.Trace
import Data.List
import Control.Monad

-- | access list elements safely
(!!) :: [a] -> Int -> Maybe a
(!!) lst idx = if idx >= length lst
                then Nothing
                else Just $ lst Prelude.!! idx

requiredPluginVersion :: String
requiredPluginVersion = "1.0"

-- | Perform parse_query extension sanity check
checkPluginVersion :: String -> IO ()
checkPluginVersion auth = do
  res <- Td.checkPlugin auth requiredPluginVersion
  case res of
    Left err -> error err
    _ -> return ()


-- | Uses the cuckoo extension to get the query-tree of a constant from psql
stringToConst :: String    -- ^ The authentication string
              -> String    -- ^ Query
              -> IO String -- ^ Parse-Tree-String
stringToConst auth q = do
    log <- getTable auth ("select string_to_const($stringtoconst$" ++ q ++ "$stringtoconst$)")
    return $ fromSql $ snd $ head $ filter (\ (i, _) -> i == "string_to_const") $ head log

-- | Gets an authStr and an I.Expr (const) and returns the O.Expr of it
parseConst :: String
           -> I.Expr
           -> IO O.Expr
parseConst authStr p@(I.CONST {I.constvalue=constvalue, I.consttype=consttype})
  = do
    when (constvalue=="" || consttype=="") $ error $ "parseConst invalid argument: " ++ show p
    checkPluginVersion authStr
    -- Use the authStr and the expr node to get the constant as query-tree
    querylog <- stringToConst authStr $ "select '" ++ constvalue ++ "' :: " ++ consttype
    -- Parse the query-tree
    let f = head $ parseLog querylog

    let finalConst = T.translate f
    return finalConst

checkAndGenerateStmt :: String -> I.PlannedStmt -> IO (O.PLANNEDSTMT, String, String)
checkAndGenerateStmt authStr op = do
    -- Validate the AST
    let errs = V.validatePlannedStmt op

    unless (null errs) $
      do
        error $ "AST is invalid:\n" ++ intercalate "\n" errs

    -- Get Catalog data
    tableDataR <- getTableData authStr

    -- Use Extract.hs to extract information from AST to be pre-transformed etc.
    let consts = E.extractP op
    -- Compile constants
    consts' <- mapM (\x -> parseConst authStr x >>= \p -> return (x, p)) $ lgconsts consts
    
    -- Infere output AST
    let infered = generatePlan tableDataR consts' (lgTableNames consts) (lgScan consts) op
    let pgplan = gprint infered
    -- putStrLn $ "Explain: "
    let s1 = "select plan_explain('" ++ pgplan ++ "', true);"
    -- putStrLn $ "Execute:"
    let s2 = "select plan_execute('" ++ pgplan ++ "');"
    return (infered, s1, s2)
