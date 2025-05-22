
module ParsePlanInterface ( parsePlan ) where

import Reader
import ParsePlan
import Database.PgCuckoo.GetTable
import Database.PgCuckoo.PgPlan as CP
import qualified Text.Show.Pretty as PP

-- | Uses the parse_query extension to get the query-tree from psql
logFromPSQL :: String    -- ^ The authentication string
            -> String    -- ^ Query
            -> IO String -- ^ Query-Tree-String
logFromPSQL auth q = do
    log <- getTable auth ("select plan_serialize($plan_serialize$" ++ q ++ "$plan_serialize$, false)")
    return $ fromSql $ snd $ head $ filter (\ (i, _) -> i == "plan_serialize") $ head log

-- | Uses the parse_query extension to parse a constant node back to its
-- AST equivalent. This is then used to use Postgres' print functions
-- to get a perfect conversion of the value.
constNodeToString :: String    -- ^ The authentication string
                  -> String    -- ^ Const query-tree format
                  -> IO String -- ^ Const as string
constNodeToString auth q = do
    log <- getTable auth ("select parse_constant_node($parseConst$" ++ q ++ "$parseConst$ :: text)")
    return $ fromSql $ snd $ head $ filter (\ (i,_) -> i == "parse_constant_node") $ head log

-- | Takes an PSQL auth-str as well as a query and returns an AST
-- The auth-str has to include:
--
--     * host=localhost
--     * dbname=scratch
--
-- aligned like: "host=localhost dbname=scratch"
--
-- More information: <https://www.postgresql.org/docs/9.6/static/libpq-connect.html>
-- Section 32.1.2. Parameter Key Words
parsePlan :: String                  -- ^ The authentication string
           -> String                 -- ^ SQL query to transform into AST
           -> IO (CP.PLANNEDSTMT, [(CP.Expr, String)])  -- ^ Generated `AST`
parsePlan authStr query = do
    -- Use the authStr and the query string to get the Querytree from Postgres
    queryLog <- logFromPSQL authStr query
    -- putStrLn queryLog
    -- Parse the querytree
    let f = head $ parseLog queryLog
    let (finalAST, consts) = parse f
    consts' <- flip mapM consts (\p -> do
                                        let (x, y) = p
                                        y' <- constNodeToString authStr y
                                        return (x, y') )
    return (finalAST, consts')
