module Lib
    ( parseConst
    ) where

import Reader as R
import TrChunks as T
import GetTable as Td
import qualified InAST as I
import qualified PgPlan as O
import Debug.Trace
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

