module Lib
    ( parseConst
    , checkAndGenerateStmt
    ) where

import Reader as R
import TrChunks as T
import GetTable as Td
import qualified InAST as I
import PgPlan as O
import qualified Validate as V
import Inference as IF
import Extract as E
import GPrint as G
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
    putStrLn $ "Validate: "
    let errs = V.validatePlannedStmt op
    -- Print errors
    -- putStrLn $ intercalate "\n" errs

    unless (null errs) $
      do
        error $ "AST is invalid:\n" ++ intercalate "\n" errs

    -- Get Catalog data
    tableDataR <- getTableData authStr

    -- Use Extract.hs to extract information from AST to be pre-transformed etc.
    let consts = E.extractP op
    putStrLn $ PP.ppShow consts

    -- Compile constants
    consts' <- mapM (\x -> parseConst authStr x >>= \p -> return (x, p)) $ lgconsts consts
    
    -- Debug output of constants
    putStrLn $ PP.ppShow consts'

    -- Infere output AST
    let infered = generatePlan tableDataR consts' (lgTableNames consts) (lgScan consts) op
    putStrLn $ PP.ppShow op
    -- Print AST structure as well as the postgres plan
    putStrLn $ PP.ppShow infered
    let pgplan = gprint infered
    -- putStrLn $ "Explain: "
    let s1 = "select _pq_plan_explain('" ++ pgplan ++ "', true);"
    -- putStrLn $ "Execute:"
    let s2 = "select _pq_plan_deserialize('" ++ pgplan ++ "');"
    return (infered, s1, s2)
