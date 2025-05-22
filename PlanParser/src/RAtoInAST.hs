{-# LANGUAGE NamedFieldPuns #-}

module RAtoInAST where

import OperSemIO
import qualified Data.List as DL
import qualified Data.Maybe as MB
import qualified Text.Show.Pretty as PP

import LogicalAST as A
import Database.PgCuckoo.InAST as O

data Context = C { boundVars :: [(String, String)] }

data StateI = StateI { phase1Dexists :: Bool }

existsD :: Rule () Bool
existsD () = do
  st <- lift $ gets phase1Dexists
  return st

phase1Dcreated :: Rule () ()
phase1Dcreated () = do
  lift $ modify (\x -> x { phase1Dexists = True })
  return ()

type Log = ()

type Rule a b = a -> OperSemIO StateI Context b Log

-- | Three parameter rule
type Rule3 a b c d = a -> b -> c -> OperSemIO StateI Context d Log

rewrite :: Plan -> IO O.PlannedStmt
rewrite p
  = do
      ((erg, lg), state) <- runOperSemSTIO (translate p)
                                            (StateI { phase1Dexists = False})
                                            (C [])
      let erg' = case erg of
                Left err -> error err
                Right f  -> f
      return erg'

translate :: Rule A.Plan O.PlannedStmt
translate p = do
  p' <- trPlan p
  return $ O.PlannedStmt { O.planTree = p' [], O.subplans = [] }

trPlan :: Rule A.Plan ([O.Expr] -> O.Operator)
trPlan A.Selection { A.predicate, A.leftTree }
  = do
    predicate' <- mapM trExpr predicate
    leftTree'  <- trPlan leftTree
    return $ const $ leftTree' predicate'

trPlan A.Constant { A.targetList }
  = do
    targetList' <- mapM trTargetEntry targetList
    return $ \x -> O.RESULT { O.targetlist = targetList'
                    , O.resconstantqual = if null x then Nothing else Just $ head x }

trPlan A.Scan { A.relation, A.alias, A.colnames }
  = do
    let targetlist = [ O.TargetEntry 
                        { O.targetexpr = 
                            O.VAR
                            { O.varTable = relation
                            , O.varColumn = col}
                        , O.targetresname = col
                        , O.resjunk = False }
                      | col <- colnames ]
    return $ \x -> O.SEQSCAN
            { O.targetlist   = targetlist
            , O.qual         = x
            , O.scanrelation = relation }

trPlan A.GroupBy { A.leftTree, A.groupCols, A.targetList }
  = do
    leftTree' <- trPlan leftTree
    targetList' <- mapM trTargetEntry targetList

    return $ \x -> O.AGG
              { O.targetlist = targetList'
              , O.qual       = x
              , O.operator   = leftTree' []
              , O.groupCols  = [] --groupCols
              , O.aggstrategy = O.AGG_PLAIN
              , O.aggsplit    = [ O.AGGSPLITOP_SIMPLE ] }

trPlan e = error $ "Not implemented for:\n" ++ PP.ppShow e

trTargetEntry :: Rule A.TargetExpr O.TargetEntry
trTargetEntry A.TargetExpr { A.expr, A.resname }
  = do
    expr' <- trExpr expr
    let resname' = MB.fromJust resname
    return O.TargetEntry
            { O.targetexpr = expr'
            , O.targetresname = resname'
            , O.resjunk = False }

trExpr :: Rule A.Expr O.Expr
trExpr A.Const { A.value, A.typ }
  = return O.CONST { O.constvalue = value, O.consttype = typ }

trExpr A.Operator { A.operator, A.args }
  = do
    args' <- mapM trExpr args

    return O.OPEXPR
            { O.oprname = operator
            , O.oprargs = args' }

-- A.VAR compilation depends on the context the expression
-- lives in. Only A.VARs that end up in the targetlist of a
-- SEQSCAN are allowed to have the actual tableName as reference.
-- In every other case we need to generate "OUTER_PLAN" or "INNER_PLAN"
trExpr A.VAR { A.tableName, A.colName }
  = return O.VAR { O.varTable = tableName, O.varColumn = colName }

trExpr A.AGG { A.aggname, A.args }
  = do
    args' <- mapM trExpr args
    let args'' = [ O.TargetEntry
                    { O.targetexpr = expr
                    , O.targetresname = aggname ++ "_" ++ (show name)
                    , O.resjunk = False } 
                  | (expr, name) <- zip args' [1..] ]
    return $ O.AGGREF
              { O.aggname = aggname
              , O.aggargs = args''
              , O.aggdirectargs = []
              , O.aggorder = []
              , O.aggdistinct = []
              , O.aggfilter = Nothing
              , O.aggstar = False }

trExpr e = error $ "Not implemented for:\n" ++ PP.ppShow e
