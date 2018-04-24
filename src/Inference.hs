{-|
Module      : Inference
Description : Performs inference rules
Author      : Denis Hirn

-}

module Inference ( generatePlan ) where

import OperSem
import qualified Text.Show.Pretty as PP
import qualified Data.Map as M
import Data.List

import GetTable as Tbl

import qualified InAST as I
import qualified PgPlan as O

import Debug.Trace
--------------------------------------------------------------------------------
-- Exported functions

generatePlan :: TableData -> [(I.Expr, O.Expr)] -> [String] -> I.Operator -> O.PLANNEDSTMT
generatePlan tableD exprs tableNames ast = let
    (stmt, lg) = runOperSem (trPlannedStmt ast tableNames) (StateI 0) (C tableD exprs [])
    res = case stmt of
            Prelude.Left str -> error $ "Inference error: " ++ str
            Prelude.Right a -> a
    in res

--------------------------------------------------------------------------------
-- READER MONAD SECTION

data Context = C { tableData :: TableData          -- ^ as provided by the GetTable module
                 , exprs     :: [(I.Expr, O.Expr)]
                 , rtables   :: [PgTable]
                 }

getRTableData :: Rule () TableData
getRTableData () = lift $ asks tableData

getExprList :: Rule () [(I.Expr, O.Expr)]
getExprList () = lift $ asks exprs

getRTables :: Rule () [PgTable]
getRTables () = lift $ asks rtables

getRTableRow :: Rule (TableData -> Tbl.Table, Table -> [Row]) [Row]
getRTableRow (tbl, idx) = do
  td <- getRTableData ()
  let ops = tbl td
  let row = idx ops
  return row

-- / READER MONAD SECTION
--------------------------------------------------------------------------------
-- WRITER MONAD SECTION

data Log = Log { errors :: [ String ] }

instance Monoid Log where
  mempty = Log []
  (Log e1) `mappend` (Log e2) = Log (e1++e2)


logError :: Rule String ()
logError err = tell (Log [err])
-- / WRITER MONAD SECTION
--------------------------------------------------------------------------------
-- STATE MONAD SECTION

data StateI = StateI { count :: Integer }

fresh :: Rule String String
fresh v = do
  n <- lift $ gets count
  lift $ modify (\x -> StateI (count x+1))
  return (v ++ show n)

freshI :: Rule () Integer
freshI () = do
  n <- lift $ gets count
  lift $ modify (\x -> StateI (count x+1))
  return n

-- / STATE MONAD SECTION
--------------------------------------------------------------------------------

-- | One parameter rule
type Rule a b = a -> OperSem StateI Context b Log

-- | Two parameter rule
type Rule2 a b c = a -> b -> OperSem StateI Context c Log

--------------------------------------------------------------------------------
-- Inference rules

trPlannedStmt :: Rule2 I.Operator [String] O.PLANNEDSTMT
trPlannedStmt op tbls = do
  tables <- mapM accessPgClass tbls
  context <- lift $ ask
  let context' = const $ context {rtables=tables}
  res <- local context' $ trOperator op

  let rtable = O.List $ map pgTableToRTE tables
  return $ O.defaultPlannedStmt { O.planTree = res, O.rtable=rtable }

pgTableToRTE :: PgTable -> O.RangeEx
pgTableToRTE t = O.RTE
                  { O.alias         = Nothing
                  , O.eref          = erefA
                  , O.rtekind       = 0
                  , O.relid         = oid
                  , O.relkind       = relkind
                  , O.tablesample   = O.Null
                  , O.lateral       = O.PgBool False
                  , O.inh           = O.PgBool False
                  , O.inFromCl      = O.PgBool True
                  , O.requiredPerms = 2
                  , O.checkAsUser   = 0
                  , O.selectedCols  = O.Bitmapset collids
                  , O.insertedCols  = O.Bitmapset []
                  , O.updatedCols   = O.Bitmapset []
                  , O.securityQuals = O.Null
                  }
  where
    collids = map cAttnum $ tCols t
    colnames = map cAttname $ tCols t
    erefA = O.Alias { O.aliasname = tName t
                    , O.colnames  = O.List colnames }
    oid = tOID t
    relkind = tKind t



trOperator :: Rule I.Operator O.Plan
trOperator (I.RESULT { I.targetlist=targetlist, I.qual=qual})
  = do
    targetlist' <- mapM trTargetEntry $ zip targetlist [1..]
    qual'       <- mapM trExpr qual
    return $ O.RESULT (O.defaultPlan {O.targetlist=O.List targetlist'}) qual'

trOperator (I.SEQSCAN { I.targetlist=targetlist
                      , I.qual=qual
                      , I.scanrelation=scanrelation })
  = do
    targetlist' <- mapM trTargetEntry $ zip targetlist [1..]
    qual'       <- mapM trExpr qual

    rtables <- getRTables ()
    let [rtable] = filter (\x -> tName x == scanrelation) rtables
    let (Just index) = elemIndex rtable rtables
    let index' = fromIntegral index + 1
    return $ O.SEQSCAN (O.defaultPlan { O.targetlist=O.List targetlist'
                                      , O.qual=qual'}) index'

trTargetEntry :: Rule (I.TargetEntry, Integer) O.TARGETENTRY
trTargetEntry (I.TargetEntry { I.targetexpr=targetexpr
                            , I.targetresname=targetresname }, resno)
  = do
    targetexpr' <- trExpr targetexpr

    return $ O.TARGETENTRY
              { O.expr            = targetexpr'
              , O.resno           = resno
              , O.resname         = Just targetresname
              , O.ressortgroupref = 0 -- Constant for now
              , O.resorigtbl      = 0
              , O.resorigcol      = 0
              , O.resjunk         = O.PgBool False
              }


trExpr :: Rule I.Expr O.Expr
trExpr c@(I.CONST {})
  = do
    exprs <- getExprList ()
    let matches = filter (\x -> c == fst x) exprs
    case matches of
      [] -> error $ "No const to infere found: " ++ PP.ppShow c
      x:_ -> return $ snd x

trExpr (I.VAR { I.varTable = varTable
              , I.varColumn = varColumn })
  = do
    rtables <- getRTables ()
    let [rtable] = filter (\x -> tName x == varTable) rtables
    let [column] = filter (\x -> cAttname x == varColumn) $ tCols rtable
    let (Just index) = elemIndex rtable rtables
    let index' = fromIntegral index + 1
    return $ O.VAR
              { O.varno       = index'
              , O.varattno    = cAttnum column
              , O.vartype     = cAtttypid column
              , O.vartypmod   = cAtttypmod column
              , O.varcollid   = cAttcollation column
              , O.varlevelsup = 0
              , O.varnoold    = index'
              , O.varoattno   = cAttnum column
              , O.location    = -1
              }

--------------------------------------------------------------------------------
-- Datatypes and functions to access pg catalog information

data PgTable = PgTable
                { tOID  :: Integer
                , tName :: String
                , tKind :: String
                , tCols :: [PgColumn]
                }
  deriving(Show, Eq)

data PgColumn = PgColumn
                { cAttname      :: String
                , cAtttypid     :: Integer
                , cAttlen       :: Integer
                , cAttnum       :: Integer
                , cAtttypmod    :: Integer
                , cAttcollation :: Integer
                }
  deriving(Show, Eq)

accessPgClass :: Rule String PgTable
accessPgClass tableName = do
  -- Get row of table in pg_class
  classRow <- getRTableRow (pg_class, findRow "relname" tableName)
  when (null classRow) $ error $ "accessPgClass no rows found"
  -- Fetch relid from row
  let relid = fromSql $ head classRow M.! "oid"
  -- Fetch relkind from row
  let relkind = fromSql $ head classRow M.! "relkind"
  -- Fetch information about columns
  cols <- accessPgAttribute relid
  return $ PgTable relid tableName relkind cols

accessPgAttribute :: Rule Integer [PgColumn]
accessPgAttribute relid = do
    -- Fetch rows from pg_attribute with information about the columns
    attRows <- getRTableRow (pg_attribute, findRow "attrelid" relid)
    return $ map rowToPgColumn attRows

rowToPgColumn :: Row -> PgColumn
rowToPgColumn r = PgColumn attname atttypid attlen attnum atttypmod attcollation
  where
    attname      = fromSql $ r M.! "attname"
    atttypid     = fromSql $ r M.! "atttypid"
    attlen       = fromSql $ r M.! "attlen"
    attnum       = fromSql $ r M.! "attnum"
    atttypmod    = fromSql $ r M.! "atttypmod"
    attcollation = fromSql $ r M.! "attcollation"

--
--------------------------------------------------------------------------------
