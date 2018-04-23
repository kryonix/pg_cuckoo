{-|
Module      : Inference
Description : Performs inference rules
Author      : Denis Hirn

-}

module Inference ( generatePlan ) where

import OperSem
import qualified Text.Show.Pretty as PP

import GetTable as Tbl

import qualified InAST as I
import qualified PgPlan as O
--------------------------------------------------------------------------------
-- Exported functions

generatePlan :: TableData -> [(I.Expr, O.Expr)] -> I.Operator -> O.PLANNEDSTMT
generatePlan tableD exprs ast = let
    (stmt, lg) = runOperSem (trOperator ast) (StateI 0) (C tableD exprs)
    res = case stmt of
            Prelude.Left str -> error $ "Inference error: " ++ str
            Prelude.Right a -> a
    in O.defaultPlannedStmt { O.planTree = res }

--------------------------------------------------------------------------------
-- READER MONAD SECTION

data Context = C { tableData :: TableData          -- ^ as provided by the GetTable module
                 , exprs     :: [(I.Expr, O.Expr)]
                 }

getRTableData :: Rule () TableData
getRTableData () = lift $ asks tableData

getExprList :: Rule () [(I.Expr, O.Expr)]
getExprList () = lift $ asks exprs

getTableRow :: Rule (TableData -> Tbl.Table, Table -> [Row]) [Row]
getTableRow (tbl, idx) = do
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

type Rule a b = a -> OperSem StateI Context b Log

--------------------------------------------------------------------------------
-- Inference rules

trOperator :: Rule I.Operator O.Plan
trOperator (I.RESULT { I.targetlist=targetlist, I.qual=qual})
  = do
    targetlist' <- mapM trTargetEntry $ zip targetlist [1..]
    qual'       <- mapM trExpr qual
    return $ O.RESULT (O.defaultPlan {O.targetlist=O.List targetlist'}) qual'

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
