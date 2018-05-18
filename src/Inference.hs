{-|
Module      : Inference
Description : Performs inference rules
Author      : Denis Hirn

-}

{-# LANGUAGE NamedFieldPuns #-}

module Inference ( generatePlan ) where

import OperSem
import qualified Text.Show.Pretty as PP
import qualified Data.Map as M
import Data.List

import GetTable as Tbl
import Data.Convertible.Base
import qualified Database.HDBC as DB
import qualified Text.Read as TR
import           Data.Maybe

import qualified InAST as I
import qualified PgPlan as O

import Debug.Trace
--------------------------------------------------------------------------------
-- Exported functions

generatePlan :: TableData -> [(I.Expr, O.Expr)] -> [String] -> [I.Operator] -> I.Operator -> O.PLANNEDSTMT
generatePlan tableD exprs tableNames valuesScan ast = let
    (stmt, lg) = runOperSem (trPlannedStmt ast tableNames valuesScan) (StateI 0) (C tableD exprs [] [])
    res = case stmt of
            Prelude.Left str -> error $ "Inference error: " ++ str
            Prelude.Right a -> a
    in res

--------------------------------------------------------------------------------
-- READER MONAD SECTION

data Context = C { tableData  :: TableData          -- ^ as provided by the GetTable module
                 , exprs      :: [(I.Expr, O.Expr)]
                 , rtables    :: [PgTable]
                 , valueScans :: [(I.Operator, Integer)]
                 }

getRTableData :: Rule () TableData
getRTableData () = lift $ asks tableData

getExprList :: Rule () [(I.Expr, O.Expr)]
getExprList () = lift $ asks exprs

getRTables :: Rule () [PgTable]
getRTables () = lift $ asks rtables

getValueScans :: Rule () [(I.Operator, Integer)]
getValueScans () = lift $ asks valueScans

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

-- | Three parameter rule
type Rule3 a b c d = a -> b -> c -> OperSem StateI Context d Log

--------------------------------------------------------------------------------
-- Inference rules

trPlannedStmt :: Rule3 I.Operator [String] [I.Operator] O.PLANNEDSTMT
trPlannedStmt op tbls valuesScan = do
  tables <- mapM accessPgClass tbls
  context <- lift $ ask

  let numTables = length tbls
  let context' = const $ context { rtables=tables
                                 , valueScans= map (\(a, b) -> (a, fromIntegral b)) 
                                                $ zip valuesScan [numTables..numTables+(length valuesScan)] }
  res <- local context' $ trOperator op

  let rte    = map pgTableToRTE tables
  let values = map scanToRTE valuesScan
  let rtable = O.List $ rte ++ values

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

scanToRTE :: I.Operator -> O.RangeEx
scanToRTE (I.VALUESSCAN {I.targetlist}) = O.RTE_VALUES
                  { O.alias = Nothing
                  , O.eref  = erefA
                  , O.rtekind = 5
                  , O.values_lists = O.Null
                  , O.coltypes = O.Null
                  , O.coltypmods = O.Null
                  , O.colcollations = O.Null
                  , O.lateral = O.PgBool False
                  , O.inh = O.PgBool False
                  , O.inFromCl = O.PgBool True
                  , O.requiredPerms = 2
                  , O.checkAsUser = 0
                  , O.selectedCols  = O.Bitmapset []
                  , O.insertedCols  = O.Bitmapset []
                  , O.updatedCols   = O.Bitmapset []
                  , O.securityQuals = O.Null
                  }
  where
    colnum = length $ targetlist
    erefA = O.Alias { O.aliasname = "VALUES"
                    , O.colnames  = O.List $ ["column"++show x | x <- [1..colnum]]
                    }

scanToRTE (I.FUNCTIONSCAN {I.targetlist, I.funcordinality})
  = O.RTE_FUNCTIONS
      { O.alias = Nothing
      , O.eref  = erefA
      , O.rtekind = 3
      , O._functions = O.Null
      , O._funcordinality = O.PgBool funcordinality
      , O.lateral = O.PgBool False
      , O.inh     = O.PgBool False
      , O.inFromCl = O.PgBool False
      , O.requiredPerms = 2
      , O.checkAsUser = 0
      , O.selectedCols  = O.Bitmapset []
      , O.insertedCols  = O.Bitmapset []
      , O.updatedCols   = O.Bitmapset []
      , O.securityQuals = O.Null
      }
    where
      colnum = length $ targetlist
      erefA = O.Alias { O.aliasname = "FUNCTION"
                      , O.colnames  = O.List $ ["column"++show x | x <- [1..colnum]]
                      }

trOperator :: Rule I.Operator O.Plan
trOperator n@(I.RESULT { I.targetlist=targetlist, I.resconstantqual})
  = do
    targetlist' <- mapM trTargetEntry $ zip targetlist [1..]
    qual'       <- mapM trExpr resconstantqual

    when (isJust qual' && getExprType (fromJust qual') /= 16)
      $ error $ "Type error: resconstantqual is not a boolean"
              ++ "\n" ++ PP.ppShow n

    return $ O.RESULT (O.defaultPlan {O.targetlist=O.List targetlist'}) qual'

trOperator n@(I.PROJECTSET { I.targetlist, I.operator })
  = do
    operator' <- trOperator operator
    context   <- lift $ ask
    -- OUTER_PLAN = first of appendplans
    -- Generate a fake table with fake columns.
    -- We need this to perform inference of VAR, referencing
    -- sub-operators.
    let fakeTable = createFakeTable "OUTER_VAR" (O.targetlist $ O.genericPlan operator')
    -- Get rid of existing fake-tables (maybe not required?)
    let rtablesC = dropWhile (\(PgTable {tOID}) -> tOID == -1) $ rtables context
    let context' = context {rtables=fakeTable:rtablesC}

    targetlist' <- local (const context') $ mapM trTargetEntry $ zip targetlist [1..]

    return $ O.PROJECTSET
              { O.genericPlan = O.defaultPlan
                                  { O.targetlist = O.List targetlist'
                                  , O.lefttree   = Just operator' }
              }

trOperator n@(I.SEQSCAN { I.targetlist, I.qual, I.scanrelation })
  = do
    targetlist' <- mapM trTargetEntry $ zip targetlist [1..]
    qual'       <- mapM trExpr qual

    when (any (\x -> getExprType x /= 16) qual')
      $ error $ "Type error: qual is not a boolean"
              ++ "\n" ++ PP.ppShow n

    rtables <- getRTables ()
    let rtable:_ = filter (\x -> tName x == scanrelation) rtables
    let (Just index) = elemIndex rtable rtables
    let index' = fromIntegral index + 1
    return $ O.SEQSCAN (O.defaultPlan { O.targetlist=O.List targetlist'
                                      , O.qual=O.List qual'}) index'

trOperator n@(I.LIMIT { I.operator, I.limitOffset, I.limitCount })
  = do
    operator'    <- trOperator operator
    limitOffset' <- mapM trExpr limitOffset
    limitCount'  <- mapM trExpr limitCount

    when (isJust limitOffset' && getExprType (fromJust limitOffset') /= 20)
      $ error $ "Type error: limitOffset is not an int8"
              ++ "\n" ++ PP.ppShow n

    when (isJust limitCount' && getExprType (fromJust limitCount') /= 20)
      $ error $ "Type error: limitCount is not an int8"
              ++ "\n" ++ PP.ppShow n

    return $ O.LIMIT (O.defaultPlan { O.targetlist=O.targetlist (O.genericPlan operator')
                                    , O.lefttree=Just operator' }) limitOffset' limitCount'

trOperator n@(I.SORT { I.targetlist, I.operator, I.sortCols })
  = do
    targetlist' <- mapM trTargetEntry $ zip targetlist [1..]
    operator'   <- trOperator operator

    let numCols = length sortCols

    when (length targetlist' < numCols)
      $ error $ "SORT: more sortCols than targetlist entries defined."

    let collations = O.PlainList $ replicate numCols 0
    let nullsFirst = O.PlainList $ map (O.PgBool . I.sortNullsFirst) sortCols

    opnos <- mapM (trSortEx targetlist') sortCols

    when (null opnos) $ error "no opnos found"

    let sortOperators = O.PlainList $ map O.sortop opnos
    let sortColIdx    = O.PlainList $ map I.sortTarget sortCols

    return $ O.SORT
              { O.genericPlan = (O.defaultPlan { O.targetlist=O.List targetlist'
                                               , O.lefttree= Just operator'})
              , O.numCols = fromIntegral numCols
              , O.sortColIdx = sortColIdx
              , O.sortOperators = sortOperators
              , O.collations = collations
              , O.nullsFirst = nullsFirst
              }

trOperator (I.GROUP { I.targetlist, I.qual, I.operator, I.groupCols})
  = do
    context <- lift $ ask
    operator' <- trOperator operator

    -- OUTER_PLAN = first of appendplans
    -- Generate a fake table with fake columns.
    -- We need this to perform inference of VAR, referencing
    -- sub-operators.
    let fakeTable = createFakeTable "OUTER_VAR" (O.targetlist $ O.genericPlan operator')
    -- Get rid of existing fake-tables (maybe not required?)
    let rtablesC = dropWhile (\(PgTable {tOID}) -> tOID == -1) $ rtables context
    let context' = context {rtables=fakeTable:rtablesC}

    targetlist' <- local (const context') $ mapM trTargetEntry $ zip targetlist [1..]

    let grpTargets = map (getExprType . O.expr)
                          $ filter (\(O.TARGETENTRY {O.ressortgroupref=x}) -> x `elem` groupCols) targetlist'

    -- Try to find function in pg_operator by name and argument types
    ops <- mapM ((liftM fst) . searchOperator) $ zip grpTargets (repeat "=")

    qual' <- mapM trExpr qual

    return $ O.GROUP
              { O.genericPlan  = (O.defaultPlan
                                  { O.targetlist= O.List targetlist'
                                  , O.lefttree = Just operator'
                                  , O.qual     = O.List qual' })
              , O.numCols      = fromIntegral (length groupCols)
              , O.grpColIdx    = O.PlainList groupCols
              , O.grpOperators = O.PlainList ops
              }

trOperator (I.APPEND {I.targetlist, I.appendplans})
  = do
    context <- lift $ ask
    appendplans' <- mapM trOperator appendplans

    -- OUTER_PLAN = first of appendplans
    -- Generate a fake table with fake columns.
    -- We need this to perform inference of VAR, referencing
    -- sub-operators.
    let fakeTable = createFakeTable "OUTER_VAR" (O.targetlist $ O.genericPlan (head appendplans'))
    -- Get rid of existing fake-tables (maybe not required?)
    let rtablesC = dropWhile (\(PgTable {tOID}) -> tOID == -1) $ rtables context
    let context' = context {rtables=fakeTable:rtablesC}

    targetlist' <- local (const context') $ mapM trTargetEntry $ zip targetlist [1..]

    return $ O.APPEND
              { O.genericPlan = (O.defaultPlan { O.targetlist = O.List targetlist' })
              , O.partitioned_rels = O.Null
              , O.appendplans = O.List appendplans'
              }

trOperator (I.MERGEAPPEND { I.targetlist, I.mergeplans, I.sortCols })
  = do
    context <- lift $ ask
    mergeplans' <- mapM trOperator mergeplans

    -- OUTER_PLAN = first of appendplans
    -- Generate a fake table with fake columns.
    -- We need this to perform inference of VAR, referencing
    -- sub-operators.
    let fakeTable = createFakeTable "OUTER_VAR" (O.targetlist $ O.genericPlan (head mergeplans'))
    -- Get rid of existing fake-tables (maybe not required?)
    let rtablesC = dropWhile (\(PgTable {tOID}) -> tOID == -1) $ rtables context
    let context' = context {rtables=fakeTable:rtablesC}

    targetlist' <- local (const context') $ mapM trTargetEntry $ zip targetlist [1..]

    let numCols = length sortCols

    when (length targetlist' < numCols)
      $ error $ "SORT: more sortCols than targetlist entries defined."

    let collations = O.PlainList $ replicate numCols 0
    let nullsFirst = O.PlainList $ map (O.PgBool . I.sortNullsFirst) sortCols

    opnos <- mapM (trSortEx targetlist') sortCols

    when (null opnos) $ error "no opnos found"

    let sortOperators = O.PlainList $ map O.sortop opnos
    let sortColIdx    = O.PlainList $ map I.sortTarget sortCols

    return $ O.MERGEAPPEND
              { O.genericPlan = (O.defaultPlan { O.targetlist = O.List targetlist' })
              , O.partitioned_rels = O.Null
              , O.mergeplans = O.List mergeplans'
              , O.numCols = fromIntegral numCols
              , O.sortColIdx = sortColIdx
              , O.sortOperators = sortOperators
              , O.collations = collations
              , O.nullsFirst = nullsFirst
              }

trOperator (I.INDEXSCAN {I.targetlist, I.qual, I.indexqual, I.indexorderby, I.indexorderasc, I.indexname, I.scanrelation})
  = do
    targetlist' <- mapM trTargetEntry $ zip targetlist [1..]
    qual'       <- mapM trExpr qual
    indexqual'  <- mapM trExpr indexqual

    let indexorderdir = if indexorderasc then 1 else -1

    rtables <- getRTables ()
    let rtable:_ = filter (\x -> tName x == scanrelation) rtables
    let (Just index) = elemIndex rtable rtables
    let index' = fromIntegral index + 1

    tbl <- accessPgClass indexname

    return $ O.INDEXSCAN
              { O.genericPlan =
                  O.defaultPlan
                    { O.targetlist = O.List targetlist'
                    , O.qual       = O.List qual'
                    }
              , O.scanrelid = index'
              , O.indexid   = tOID tbl
              , O.indexqual = O.List indexqual'
              , O.indexqualorig = O.Null
              , O.indexorderby  = Nothing
              , O.indexorderbyorig = O.Null
              , O.indexorderbyops = Nothing
              , O.indexorderdir = indexorderdir
              }

trOperator (I.INDEXONLYSCAN {I.targetlist, I.qual, I.indexqual, I.indexorderby, I.indexorderasc, I.indexname, I.scanrelation})
  = do
    context <- lift $ ask
    table <- accessPgClass indexname

    let context' = context {rtables=table:rtables context}

    targetlist' <- local (const context') $ mapM trTargetEntry $ zip targetlist [1..]
    qual'       <- local (const context') $ mapM trExpr qual
    indexqual'  <- local (const context') $ mapM trExpr indexqual

    let indexorderdir = if indexorderasc then 1 else -1

    rtables <- getRTables ()
    let rtable:_ = filter (\x -> tName x == scanrelation) rtables
    let (Just index) = elemIndex rtable rtables
    let index' = fromIntegral index + 1
    let origtbl = tOID rtable

    tbl <- accessPgClass indexname
    let indexColumns = zip [1..] (tCols tbl)

    -- Have to infer '*' from index.
    let indextlist = [ O.TARGETENTRY
                      { O.expr =
                        O.VAR
                        { O.varno = index'
                        , O.varattno = n
                        , O.vartype = cAtttypid e
                        , O.vartypmod = cAtttypmod e
                        , O.varcollid = cAttcollation e
                        , O.varlevelsup = 0
                        , O.varnoold = 0
                        , O.varoattno = 0
                        , O.location = -1
                        }
                      , O.resno = n
                      , O.resname = Nothing
                      , O.ressortgroupref = 0
                      , O.resorigtbl = 0
                      , O.resorigcol = 0
                      , O.resjunk = O.PgBool False }
                    | (n, e) <- indexColumns
                    ]

    return $ O.INDEXONLYSCAN
              { O.genericPlan =
                  O.defaultPlan
                    { O.targetlist = O.List $ map (\x -> x {O.resorigtbl=origtbl}) targetlist'
                    , O.qual       = O.List qual'
                    }
              , O.scanrelid = index'
              , O.indexid   = tOID tbl
              , O.indexqual = O.List indexqual'
              , O.indexorderby  = Nothing
              , O.indextlist = O.List indextlist
              , O.indexorderdir = indexorderdir
              }


trOperator (I.BITMAPINDEXSCAN {I.indexqual, I.indexname, I.scanrelation})
  = do
    context <- lift $ ask
    table <- accessPgClass indexname

    let context' = context {rtables=table:rtables context}

    indexqual'  <- local (const context') $ mapM trExpr indexqual

    rtables <- getRTables ()
    let rtable:_ = filter (\x -> tName x == scanrelation) rtables
    let (Just index) = elemIndex rtable rtables
    let index' = fromIntegral index + 1
    let origtbl = tOID rtable

    tbl <- accessPgClass indexname
    let indexColumns = zip [1..] (tCols tbl)

    return $ O.BITMAPINDEXSCAN
            { O.genericPlan = O.defaultPlan
            , O.scanrelid = index'
            , O.indexid   = tOID tbl
            , O.isshared  = O.PgBool False
            , O.indexqual = O.List indexqual'
            , O.indexqualorig = O.Null
            }

trOperator (I.BITMAPHEAPSCAN {I.targetlist, I.bitmapqualorig, I.operator, I.scanrelation})
  = do
    context <- lift $ ask
    operator' <- trOperator operator

    -- OUTER_PLAN = first of appendplans
    -- Generate a fake table with fake columns.
    -- We need this to perform inference of VAR, referencing
    -- sub-operators.
    let fakeTable = createFakeTable "OUTER_VAR" (O.targetlist $ O.genericPlan operator')
    -- Get rid of existing fake-tables (maybe not required?)
    let rtablesC = dropWhile (\(PgTable {tOID}) -> tOID == -1) $ rtables context
    let context' = context {rtables=fakeTable:rtablesC}

    targetlist' <- local (const context') $ mapM trTargetEntry $ zip targetlist [1..]

    bitmapqualorig' <- local (const context') $ mapM trExpr bitmapqualorig

    rtables <- getRTables ()
    let rtable:_ = filter (\x -> tName x == scanrelation) rtables
    let (Just index) = elemIndex rtable rtables
    let index' = fromIntegral index + 1
    let origtbl = tOID rtable

    return $ O.BITMAPHEAPSCAN
              { O.genericPlan =
                  O.defaultPlan
                    { O.targetlist = O.List $ targetlist'
                    , O.lefttree   = Just operator'
                    }
              , O.scanrelid = index'
              , O.bitmapqualorig = O.List bitmapqualorig'}

trOperator (I.AGG {I.targetlist, I.operator, I.groupCols})
  = do
    context <- lift $ ask
    operator' <- trOperator operator


    -- OUTER_PLAN = first of appendplans
    -- Generate a fake table with fake columns.
    -- We need this to perform inference of VAR, referencing
    -- sub-operators.
    let fakeTable = createFakeTable "OUTER_VAR" (O.targetlist $ O.genericPlan operator')
    -- Get rid of existing fake-tables (maybe not required?)
    let rtablesC = dropWhile (\(PgTable {tOID}) -> tOID == -1) $ rtables context
    let context' = context {rtables=fakeTable:rtablesC}

    targetlist' <- local (const context') $ mapM trTargetEntry $ zip targetlist [1..]


    let grpTargets = map (getExprType . O.expr)
                          $ filter (\(O.TARGETENTRY {O.ressortgroupref=x}) -> x `elem` groupCols) targetlist'

    -- Try to find function in pg_operator by name and argument types
    ops <- mapM ((liftM fst) . searchOperator) $ zip grpTargets (repeat "=")

    return $ O.AGG
              { O.genericPlan  = (O.defaultPlan { O.targetlist= O.List targetlist'
                                                , O.lefttree = Just operator'})
              , O.aggstrategy  = if (null groupCols) then 0 else 2
              , O.aggsplit     = 0
              , O.numCols      = fromIntegral (length groupCols)
              , O.grpColIdx    = O.PlainList groupCols
              , O.grpOperators = O.PlainList ops
              , O.numGroups    = 1
              , O.aggParams    = O.Bitmapset []
              , O.groupingSets = O.Null
              , O.chain        = O.Null
              }

trOperator (I.MATERIAL {I.operator})
  = do
    operator' <- trOperator operator

    let genPlan = zip [1..] $ ( (\(O.List x) -> x) . O.targetlist . O.genericPlan) operator'
    -- Take targetlist of sub-plan to generate references to the columns.
    let targetlist' = map (\(n, O.TARGETENTRY{O.expr=e, O.resname=resname}) ->
                              O.TARGETENTRY
                                { O.expr=
                                    O.VAR
                                      { O.varno       = 65001
                                      , O.varattno    = n
                                      , O.vartype     = getExprType e
                                      , O.vartypmod   = -1
                                      , O.varcollid   = 0
                                      , O.varlevelsup = 0
                                      , O.varnoold    = n
                                      , O.varoattno   = n
                                      , O.location    = -1
                                      }
                                , O.resno = n
                                , O.resname = resname
                                , O.ressortgroupref = 0
                                , O.resorigtbl = 0
                                , O.resorigcol = 0
                                , O.resjunk = O.PgBool False
                                } ) genPlan

    return $ O.MATERIAL
              { O.genericPlan =
                  (O.defaultPlan { O.targetlist = O.List targetlist'
                                 , O.lefttree = Just operator'
                                 })
              }

trOperator (I.NESTLOOP {I.targetlist, I.joinType, I.inner_unique, I.joinquals, I.nestParams, I.lefttree, I.righttree})
  = do
    lefttree' <- trOperator lefttree
    righttree' <- trOperator righttree

    context <- lift $ ask

    let fakeOuter = createFakeTable "OUTER_VAR" (O.targetlist $ O.genericPlan lefttree')
    let fakeInner = createFakeTable "INNER_VAR" (O.targetlist $ O.genericPlan righttree')
    let fakeTables = [fakeOuter, fakeInner]

    -- Get rid of existing fake-tables (maybe not required?)
    let rtablesC = dropWhile (\(PgTable {tOID}) -> tOID == -1) $ rtables context
    let context' = context {rtables=fakeTables++rtablesC}

    targetlist' <- local (const context') $ mapM trTargetEntry $ zip targetlist [1..]
    joinquals'  <- local (const context') $ mapM trExpr joinquals

    let joinType' = case joinType of
                      I.INNER -> 0
                      I.LEFT  -> 1
                      I.FULL  -> 2
                      I.RIGHT -> 3
                      I.SEMI  -> 4
                      I.ANTI  -> 5

    return $ O.NESTLOOP
              { O.genericPlan = O.defaultPlan 
                                  { O.targetlist = O.List targetlist'
                                  , O.lefttree = Just lefttree'
                                  , O.righttree = Just righttree'
                                  }
              , O.jointype = joinType'
              , O.inner_unique = O.PgBool inner_unique
              , O.joinquals = O.List joinquals'
              , O.nestParams = O.List []
              }

trOperator (I.UNIQUE {I.operator, I.uniqueCols})
  = do
    operator' <- trOperator operator
    let targetlist = ((\(O.List x) -> x) . O.targetlist . O.genericPlan) operator'

    let grpTargets = map (getExprType . O.expr)
                      $ filter (\(O.TARGETENTRY {O.ressortgroupref=x}) -> x `elem` uniqueCols) targetlist

    -- Try to find function in pg_operator by name and argument types
    ops <- mapM ((liftM fst) . searchOperator) $ zip grpTargets (repeat "=")

    return $ O.UNIQUE
              { O.genericPlan = O.defaultPlan 
                                { O.targetlist = O.List targetlist
                                , O.lefttree = Just operator' }
              , O.numCols = fromIntegral $ length uniqueCols
              , O.uniqColIdx = O.PlainList uniqueCols
              , O.uniqOperators = O.PlainList ops
              }

trOperator t@(I.FUNCTIONSCAN {I.targetlist, I.qual, I.functions, I.funcordinality})
  = do
    qual' <- mapM trExpr qual
    functions' <- mapM trExpr functions

    let functions'' = map
                      (\x ->
                          O.RANGETBLFUNCTION
                          { O.funcexpr = x
                          , O.funccolcount = 1
                          , O.funccolnames = O.Null
                          , O.funccoltypes = O.Null
                          , O.funccoltypmods = O.Null
                          , O.funccolcollations = O.Null
                          , O.funcparams = O.Bitmapset [] }) functions'

    targetlist' <- forM (zip targetlist [1..]) 
                $ \x -> do
                  let (e, n) = x
                  case e of
                    (I.TargetEntry
                      { I.targetexpr = I.SCANVAR {I.colPos}
                      , I.targetresname
                      , I.resjunk
                      }) -> do
                            let expr' = O.VAR
                                        { O.varno = n
                                        , O.varattno = colPos
                                        , O.vartype  = getExprType $ functions' !! (fromIntegral colPos-1)
                                        , O.vartypmod = -1
                                        , O.varcollid = 0
                                        , O.varlevelsup = 0
                                        , O.varnoold = n
                                        , O.varoattno = colPos
                                        , O.location = -1 }
                            return $ (O.TARGETENTRY
                                      { O.expr = expr'
                                      , O.resno = n
                                      , O.resname = Just targetresname
                                      , O.ressortgroupref = n
                                      , O.resorigcol = 0
                                      , O.resorigtbl = 0
                                      , O.resjunk = O.PgBool resjunk
                                      })
                    _ -> trTargetEntry (e, n)

    vscans <- getValueScans ()
    let rel:_ = filter ((==t) . fst) vscans
    let relid = snd rel

    return $ O.FUNCTIONSCAN
              { O.genericPlan = O.defaultPlan
                                  { O.targetlist = O.List targetlist'
                                  , O.qual = O.List qual'}
              , O.scanrelid = relid
              , O.functions = O.List functions''
              , O.funcordinality = O.PgBool funcordinality
              }

trOperator s@(I.VALUESSCAN {I.targetlist, I.qual, I.values_list})
  = do
    qual'       <- mapM trExpr qual
    values_list' <- mapM (mapM trExpr) values_list

    targetlist' <- forM (zip targetlist [1..]) 
                    $ \x -> do
                      let (e, n) = x
                      case e of
                        (I.TargetEntry
                          { I.targetexpr = I.SCANVAR {I.colPos}
                          , I.targetresname
                          , I.resjunk
                          }) -> do
                                let expr' = O.VAR
                                            { O.varno = n
                                            , O.varattno = colPos
                                            , O.vartype  = getExprType $ (head values_list') !! (fromIntegral colPos-1)
                                            , O.vartypmod = -1
                                            , O.varcollid = 0
                                            , O.varlevelsup = 0
                                            , O.varnoold = n
                                            , O.varoattno = colPos
                                            , O.location = -1 }
                                return $ (O.TARGETENTRY
                                          { O.expr = expr'
                                          , O.resno = n
                                          , O.resname = Just targetresname
                                          , O.ressortgroupref = n
                                          , O.resorigcol = 0
                                          , O.resorigtbl = 0
                                          , O.resjunk = O.PgBool resjunk
                                          })
                        _ -> trTargetEntry (e, n)

    vscans <- getValueScans ()
    let rel:_ = filter ((==s) . fst) vscans
    let relid = snd rel

    return $ O.VALUESSCAN
              { O.genericPlan = O.defaultPlan { O.targetlist = O.List targetlist'
                                              , O.qual = O.List qual' }
              , O.scanrelid = relid
              , O.values_list = O.List $ map O.List values_list'
              }

trOperator s@(I.HASH {I.targetlist, I.qual, I.operator, I.skewTable, I.skewColumn})
  = do
    context <- lift $ ask
    --rtables <- getRTables ()
    operator' <- trOperator operator

    -- OUTER_PLAN = first of appendplans
    -- Generate a fake table with fake columns.
    -- We need this to perform inference of VAR, referencing
    -- sub-operators.
    let fakeTable = createFakeTable "OUTER_VAR" (O.targetlist $ O.genericPlan operator')
    -- Get rid of existing fake-tables (maybe not required?)
    let rtablesC = dropWhile (\(PgTable {tOID}) -> tOID == -1) $ rtables context
    let context' = context {rtables=fakeTable:rtablesC}

    targetlist' <- local (const context') $ mapM trTargetEntry $ zip targetlist [1..]

    qual' <- local (const context') $ mapM trExpr qual

    let rtable:_ = filter (\x -> tName x == skewTable) rtablesC

    return $ O.HASH
              { O.genericPlan = O.defaultPlan
                                { O.targetlist = O.List targetlist'
                                , O.qual       = O.List qual'
                                , O.lefttree   = Just operator'
                                }
              , O.skewTable = tOID rtable
              , O.skewColumn = skewColumn
              , O.skewInherit = O.PgBool False
              }

trOperator s@(I.HASHJOIN {I.targetlist, I.joinType, I.inner_unique, I.joinquals, I.hashclauses, I.lefttree, I.righttree})
  = do
    lefttree' <- trOperator lefttree
    righttree' <- trOperator righttree

    context <- lift $ ask

    let fakeOuter = createFakeTable "OUTER_VAR" (O.targetlist $ O.genericPlan lefttree')
    let fakeInner = createFakeTable "INNER_VAR" (O.targetlist $ O.genericPlan righttree')
    let fakeTables = [fakeOuter, fakeInner]

    -- Get rid of existing fake-tables (maybe not required?)
    let rtablesC = dropWhile (\(PgTable {tOID}) -> tOID == -1) $ rtables context
    let context' = context {rtables=fakeTables++rtablesC}

    targetlist' <- local (const context') $ mapM trTargetEntry $ zip targetlist [1..]
    hashclauses'  <- local (const context') $ mapM trExpr hashclauses
    joinquals'  <- local (const context') $ mapM trExpr joinquals

    let joinType' = case joinType of
                  I.INNER -> 0
                  I.LEFT  -> 1
                  I.FULL  -> 2
                  I.RIGHT -> 3
                  I.SEMI  -> 4
                  I.ANTI  -> 5

    return $ O.HASHJOIN
              { O.genericPlan = O.defaultPlan
                                  { O.targetlist = O.List targetlist'
                                  , O.lefttree = Just lefttree'
                                  , O.righttree = Just righttree'
                                  }
              , O.jointype = joinType'
              , O.inner_unique = O.PgBool inner_unique
              , O.joinqual = O.List joinquals'
              , O.hashclauses = O.List hashclauses'
              }

-- | Takes a list of targetentries and a name to generate a fake table
-- This table is used for VAR "{INNER,OUTER}_VAR" references.
createFakeTable :: String -> (O.List O.TARGETENTRY) -> PgTable
createFakeTable name (O.List targets)
  = PgTable { tOID = -1, tName = name, tKind = "", tCols = fakeCols }
    where
      fakeCols = [ PgColumn
                    { cAttname      = maybe "" id resname
                    , cAtttypid     = getExprType expr
                    , cAttlen       = 0
                    , cAttnum       = num
                    , cAtttypmod    = -1
                    , cAttcollation = 0
                    }
                 |
                  (O.TARGETENTRY { O.expr=expr, O.resname }, num) <- zip targets [1..]
                 ]

trSortEx :: Rule2 [O.TARGETENTRY] I.SortEx O.SORTGROUPCLAUSE
trSortEx targetlist (I.SortEx { I.sortTarget, I.sortASC, I.sortNullsFirst })
  = do
    let targetExpr:_ = filter ((==sortTarget) . O.ressortgroupref) targetlist
    let targetType = getExprType $ O.expr targetExpr
    let targetSortop = if sortASC then "<" else ">"

    opno <- liftM fst $ searchOperator (targetType, targetSortop)

    (eqop, hashable) <- searchOperator (targetType, "=")

    return $ O.SORTGROUPCLAUSE
                { O.tleSortGroupRef = sortTarget
                , O.eqop = eqop
                , O.sortop = opno
                , O.nulls_first = O.PgBool sortNullsFirst
                , O.hashable = O.PgBool hashable }

searchOperator :: Rule (Integer, String) (Integer, Bool)
searchOperator (typ, op)
  = do
    -- Get tables from context, we have to do it that way instead of via
    -- findRow, because we need to perform a search using multiple qualifications.
    td <- getRTableData ()
    let table = pg_operators td
    
    -- Try to find function in pg_operator by name and argument types
    let row = filter (\x -> x M.! "oprname" == (DB.toSql op)
                         && x M.! "oprleft" == (DB.toSql typ)
                         && x M.! "oprright" == (DB.toSql typ)) table

    -- No operator matching the argument types or operator name exists.
    when (null row)
      $ error $ "searchOperator: no operator found"

    let opno       = fromSql $ head row M.! "oid"
    let oprcanhash = fromSql $ head row M.! "oprcanhash"
    return (opno, oprcanhash)


trTargetEntry :: Rule (I.TargetEntry, Integer) O.TARGETENTRY
trTargetEntry (I.TargetEntry { I.targetexpr, I.targetresname, I.resjunk }, resno)
  = do
    targetexpr' <- trExpr targetexpr

    return $ O.TARGETENTRY
              { O.expr            = targetexpr'
              , O.resno           = resno
              , O.resname         = Just targetresname
              , O.ressortgroupref = resno -- 0 -- Constant for now
              , O.resorigtbl      = 0
              , O.resorigcol      = 0
              , O.resjunk         = O.PgBool resjunk
              }

trExpr :: Rule I.Expr O.Expr
trExpr c@(I.CONST {})
  = do
    exprs <- getExprList ()
    let matches = filter (\x -> c == fst x) exprs
    case matches of
      [] -> error $ "No const to infer found: " ++ PP.ppShow c
      x:_ -> return $ snd x


-- INNER_VAR ref: 65000
-- OUTER_VAR ref: 65001
-- INDEX_VAR ref: 65002
trExpr v@(I.VAR { I.varTable, I.varColumn })
  = do
    rtables <- getRTables ()
    let rtable' = filter (\x -> tName x == varTable) rtables
    let rtable = case rtable' of
                  r:_ -> r
                  err -> error $ "No table found: " ++ PP.ppShow v ++ "\n" ++ PP.ppShow rtables
    let column' = filter (\x -> cAttname x == varColumn) $ tCols rtable
    let column = case column' of
                  c:_ -> c
                  err -> error $ "No column found: " ++ PP.ppShow v ++ "\n" ++ PP.ppShow rtables
    let tableKind = tKind rtable
    -- Calculate the index of the table in rtable of PlannedStmt
    let (Just index) = elemIndex rtable rtables
    -- Postgres enumerates these indizes from 1, so we have to increment the value
    let index' = case varTable of
                  "INNER_VAR" -> 65000
                  "OUTER_VAR" -> 65001
                  "INDEX_VAR" -> 65002
                  _ -> if tableKind == "i" then 65002 else fromIntegral index + 1

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

trExpr n@(I.FUNCEXPR { I.funcname, I.funcargs })
  = do
    funcargs' <- mapM trExpr funcargs

    -- Type check
    let argTypes = map getExprType funcargs'

    -- Get tables from context, we have to do it that way instead of via
    -- findRow, because we need to perform a search using multiple qualifications.
    td <- getRTableData ()
    let table = pg_proc td
    -- Try to find function in pg_operator by name and argument types
    let procrow = filter (\x -> x M.! "proname" == (DB.toSql funcname)
                             && x M.! "pronargs" == (DB.toSql (length funcargs))) table

    -- Error if the function does not exist
    when (null procrow) $ error $ "FUNCEXPR: function " ++ funcname ++ " not found."
    -- Extract all information we need
    let prooid      = fromSql $ head procrow M.! "oid"
    let prorettype  = fromSql $ head procrow M.! "prorettype"
    let proretset   = fromSql $ head procrow M.! "proretset"
    let provariadic = fromSql $ head procrow M.! "provariadic"
    let pronargs    = fromSql $ head procrow M.! "pronargs"

    -- Check if argument count is correct. Error if mismatch
    when (pronargs /= length funcargs)
      $ error $ "FUNCEXPR error: expected "
                ++ show pronargs 
                ++ " arguments but got "
                ++ show (length funcargs)
                ++ "\n" ++ PP.ppShow n

    -- Query argument types for type check
    let proallargtypesDB = DB.safeFromSql $ head procrow M.! "proallargtypes" :: ConvertResult String
    let proallargtypes = case proallargtypesDB of
                            -- Second chance if conversion of proallargtypes fails.
                            Left x -> fromSql $ head procrow M.! "proargtypes"
                            Right x -> x

    let proallargtypes' = map TR.readMaybe $ words proallargtypes :: [Maybe Integer]
    unless (all isJust proallargtypes') $ error $ "getProcData error while reading proallargtypes: " ++ show procrow
    let proallargtypesSplit = catMaybes proallargtypes'

    -- Check which (if any) argument types do not match.
    -- Used for helpful error message
    let errPositions = [ (c, b, a)
                       | (a, b, c) <- zip3 argTypes proallargtypesSplit [1..]
                       , a /= b ]

    -- Types do not match, print error and abort
    unless (proallargtypesSplit == argTypes)
      $ error $ "FUNCEXPR error: type of arguments don't match function definition"
                ++ "\n"
                ++ concat [ "Argument " ++ show a ++ ": expected type " ++ show b ++ " but got " ++ show c ++ "\n"
                   | (a, b, c) <- errPositions
                   ]
                ++ PP.ppShow n

    return $ O.FUNCEXPR
              { O.funcid = prooid
              , O.funcresulttype = prorettype
              , O.funcretset = O.PgBool proretset
              , O.funcvariadic = O.PgBool provariadic
              , O.funcformat = 0   -- relevant?
              , O.funccollid = 0   -- ignore collations
              , O.inputcollid = 0  -- ignore collations
              , O.args = O.List funcargs'
              , O.location = -1
              }

trExpr n@(I.OPEXPR { I.oprname, I.oprargs })
  = do
    args' <- mapM trExpr oprargs
    let argTypes = map getExprType args'
    let (oprleft, oprright) = case argTypes of
                              [r]    -> (0, r)
                              [l, r] -> (l, r)
                              err    -> error $ "OPEXPR error: invalid number of arguments"
    -- Get tables from context, we have to do it that way instead of via
    -- findRow, because we need to perform a search using multiple qualifications.
    td <- getRTableData ()
    let table = pg_operators td
    -- Try to find function in pg_operator by name and argument types
    let row = filter (\x -> x M.! "oprname" == (DB.toSql oprname)
                         && x M.! "oprleft" == (DB.toSql oprleft)
                         && x M.! "oprright" == (DB.toSql oprright)) table

    -- No operator matching the argument types or operator name exists.
    when (null row)
      $ error $ "OPEXPR error: operator '"
                ++ oprname
                ++ "' for given arguments does not exist!"
                ++ " Check types of arguments."
                ++ "\n" ++ PP.ppShow n
    
    let oprcode = fromSql $ head row M.! "oprcode"
    -- Try to find function in pg_proc by name
    procrow <- getRTableRow (pg_proc, findRow "proname" oprcode)
    -- Error if the function does not exist (can not happen)
    when (null procrow) $ error $ "OPEXPR: function " ++ oprcode ++ " not found."
    -- Extract all information we need
    let prooid      = fromSql $ head procrow M.! "oid"
    let prorettype  = fromSql $ head procrow M.! "prorettype"
    let proretset   = fromSql $ head procrow M.! "proretset"

    let opno      = fromSql $ head row M.! "oid"

    return $ O.OPEXPR
              { O.opno = opno
              , O.opfuncid = prooid
              , O.opresulttype = prorettype
              , O.opretset = O.PgBool proretset
              , O.opcollid = 0  -- Ignore collations
              , O.inputcollid = 0
              , O.args = O.List args'
              , O.location = -1
              }

trExpr n@(I.AGGREF {I.aggname, I.aggargs, I.aggdirectargs, I.aggorder, I.aggdistinct, I.aggfilter, I.aggstar})
  = do
    -- Compile args first, because we need the types to select the row in pg_proc
    args' <- mapM trTargetEntry $ zip aggargs [1..]
    let argTypes = map (getExprType . O.expr) args'

    let proallargtypesDB x = DB.safeFromSql $ x M.! "proallargtypes" :: ConvertResult String
    let proallargtypes p = case proallargtypesDB p of
                            -- Second chance if conversion of proallargtypes fails.
                            Left x -> fromSql $ p M.! "proargtypes"
                            Right x -> x

    let proallargtypesSplit x = if (all isJust proallargtypes')
                                  then catMaybes proallargtypes'
                                  else error $ "getProcData error while reading proallargtypes: " ++ show x
          where proallargtypes' = (map TR.readMaybe $ words (proallargtypes x) :: [Maybe Integer])
    -- Get tables from context, we have to do it that way instead of via
    -- findRow, because we need to perform a search using multiple qualifications.
    td <- getRTableData ()
    let table = pg_proc td
    -- Try to find function in pg_operator by name and argument types
    let procrow = filter (\x -> x M.! "proname" == (DB.toSql aggname)
                         && x M.! "pronargs" == (DB.toSql (length args'))
                         && x M.! "proisagg" == (DB.toSql True)
                         && proallargtypesSplit x == argTypes) table

    -- Error if the function does not exist
    when (null procrow) $ error $ "AGGREF: aggregate " ++ aggname ++ " not found."
    -- Extract all information we need
    let prooid      = fromSql $ head procrow M.! "oid"
    let prorettype  = fromSql $ head procrow M.! "prorettype"
    -- let proretset   = fromSql $ head procrow M.! "proretset"
    -- let provariadic = fromSql $ head procrow M.! "provariadic"
    let pronargs    = fromSql $ head procrow M.! "pronargs"

    -- Check if argument count is correct. Error if mismatch
    when (pronargs /= length aggargs)
      $ error $ "AGGREF error: expected "
                ++ show pronargs 
                ++ " arguments but got "
                ++ show (length aggargs)
                ++ "\n" ++ PP.ppShow n

    -- Try to find function in pg_proc by name
    aggrow <- getRTableRow (pg_aggregate, findRow "oid" prooid)

    when (null aggrow)
      $ error $ "AGGREF error: aggrow not found: " ++ show prooid

    let aggtranstype = fromSql $ head aggrow M.! "aggtranstype"
    let aggkind      = fromSql $ head aggrow M.! "aggkind"

    -- Query argument types for type check
    let aggargtypes = O.RelationList $ proallargtypesSplit (head procrow)

    aggdirectargs' <- mapM trExpr aggdirectargs
    aggfilter'     <- mapM trExpr aggfilter

    aggorder'    <- mapM (trSortEx args') aggorder
    aggdistinct' <- mapM (trSortEx args') aggdistinct

    return $ O.AGGREF
            { O.aggfnoid      = prooid
            , O.aggtype       = prorettype
            , O.aggcollid     = 0
            , O.inputcollid   = 0
            , O.aggtranstype  = aggtranstype
            , O.aggargtypes   = aggargtypes
            , O.aggdirectargs = O.List aggdirectargs'
            , O._args         = O.List args'
            , O.aggorder      = O.List aggorder'
            , O.aggdistinct   = O.List aggdistinct'
            , O.aggfilter     = aggfilter'
            , O.aggstar       = O.PgBool aggstar
            , O.aggvariadic   = O.PgBool False
            , O.aggkind       = aggkind
            , O.agglevelsup   = 0
            , O._aggsplit     = 0
            , O.location      = -1
            }

trExpr (I.AND { I.args })
  = do
    args' <- mapM trExpr args
    return $ O.BOOLEXPR
            { O.boolop   = "and"
            , O.args     = O.List args'
            , O.location = -1
            }

trExpr (I.OR { I.args })
  = do
    args' <- mapM trExpr args
    return $ O.BOOLEXPR
            { O.boolop   = "or"
            , O.args     = O.List args'
            , O.location = -1
            }

trExpr (I.NOT { I.arg })
  = do
    arg' <- trExpr arg
    return $ O.BOOLEXPR
            { O.boolop   = "not"
            , O.args     = O.List [arg']
            , O.location = -1
            }

--------------------------------------------------------------------------------
--

getExprType :: O.Expr -> Integer
getExprType (O.VAR { O.vartype }) = vartype
getExprType (O.CONST { O.consttype }) = consttype
getExprType (O.FUNCEXPR { O.funcresulttype }) = funcresulttype
getExprType (O.OPEXPR { O.opresulttype }) = opresulttype
getExprType (O.AGGREF { O.aggtype }) = aggtype

getExprType x = error $ "getExprType not implemented for: " ++ PP.ppShow x

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
  when (null classRow) $ error $ "accessPgClass no rows found: " ++ tableName ++ " does not exist"
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
