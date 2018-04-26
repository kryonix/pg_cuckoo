{-|
Module      : GetTable
Description : This module extracts data from PSQL
Copyright   : © Denis Hirn <denis.hirn@student.uni-tuebingen.de>
License     : AllRightsReserved
Maintainer  : Denis Hirn
-}

{-# LANGUAGE FlexibleContexts #-}

module GetTable( getTable
               , getTableData
               , findRow
               , fromSql
               , checkPlugin
               , Table
               , Row
               , TableData(..)
               ) where

import Data.Convertible
import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)

import qualified Data.Map as M
import qualified Data.List as L

type Row = M.Map String SqlValue
type Table = [Row]

data TableData = TableData
               { pg_operators :: Table
               , pg_type      :: Table
               , pg_proc      :: Table
               , pg_class     :: Table
               , pg_attribute :: Table
               }
    deriving (Show)

-- | Get rows that match oid
findRow :: (Convertible a SqlValue) => String -> a -> Table -> [Row]
findRow idx val t = filter (\x -> x M.! idx == (toSql val)) t

-- | Transform a row into a Data.Map
rowToMap :: [(String, SqlValue)] -> Row
rowToMap row = M.fromList row

-- | Transform a table into a list of rows
tableToMap :: [[(String, SqlValue)]] -> Table
tableToMap rows = map rowToMap rows

-- | Fetch tables from DB
getTableData :: String -> IO TableData
getTableData auth = do
    pg_operators <- getTable auth "SELECT oid, oprname, oprleft, oprright, oprresult, oprcode FROM pg_operator"
    pg_type      <- getTable auth "SELECT oid, typname, typcategory, typelem, typrelid FROM pg_type"
    pg_proc      <- getTable auth "SELECT oid, proname, proargtypes, prorettype, proretset, array_to_string(proallargtypes , ' ') as proallargtypes, array_to_string(proargmodes, ' ') as proargmodes, array_to_string(proargnames, ' ') as proargnames, provariadic=0 as provariadic, pronargs FROM pg_proc"
    pg_class     <- getTable auth "SELECT oid, relname, relkind FROM pg_class"
    pg_attribute <- getTable auth "SELECT attrelid, attnum, atttypid, attname, attlen, atttypmod, attcollation FROM pg_attribute WHERE attnum > 0"

    let tOperators = tableToMap pg_operators
    let tType      = tableToMap pg_type
    let tProc      = tableToMap pg_proc
    let tClass     = tableToMap pg_class
    let tAttribute = tableToMap pg_attribute
    return $! TableData tOperators tType tProc tClass tAttribute

checkPlugin :: String -> String -> IO (Either String ())
checkPlugin auth expected = do
    let query = "select extname, extversion from pg_extension where extname = 'cuckoo' limit 1"

    res <- getTable auth query
    let res' = tableToMap res
    case res' of
      []  -> return $ Left "ERROR: parse_query is not installed!"
      x:_ -> do
          let version = fromSql $ x M.! "extversion" :: String
          if version == expected
            then return $ Right ()
            else return $ Left $ "ERROR: parse_query version mismatch. Expected version: "
                              ++ expected ++ " but got: " ++ version

-- | Execute a query and return the result
getTable :: String -> String -> IO [[(String, SqlValue)]]
getTable auth query = do
    conn <- handleSql (fail . seErrorMsg) $ connectPostgreSQL auth
    stmt <- prepare conn $ query
    _ <- handleSql (fail . seErrorMsg) $ execute stmt []
    res <- fetchAllRowsAL stmt
    return res
