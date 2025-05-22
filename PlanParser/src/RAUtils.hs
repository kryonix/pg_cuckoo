{-# LANGUAGE NamedFieldPuns #-}

module RAUtils where

import qualified LogicalAST as A
import qualified Database.PgCuckoo.PgPlan as P

hasSubPlan :: P.Expr -> Bool
hasSubPlan (P.FUNCEXPR {P.args = P.List xs}) = any hasSubPlan xs
hasSubPlan (P.OPEXPR {P.args = P.List xs})   = any hasSubPlan xs
hasSubPlan (P.NULLTEST {P.arg}) = hasSubPlan arg
hasSubPlan (P.SUBPLAN {}) = True
hasSubPlan _              = False

trJoinType :: Integer -> A.JoinType
trJoinType 0 = A.INNER
trJoinType 1 = A.LEFT
trJoinType 2 = A.FULL
trJoinType 3 = A.RIGHT
trJoinType 4 = A.SEMI
trJoinType 5 = A.ANTI

substitute :: [ A.TargetExpr ] -> [ (String, String) ] -> [ A.TargetExpr ]
substitute tl c = map (flip substitute' c) tl

substitute' :: A.TargetExpr -> [ (String, String) ] -> A.TargetExpr
substitute' p@(A.TargetExpr { A.expr }) c = p {A.expr = substE expr c}

substE :: A.Expr -> [ (String, String) ] -> A.Expr
substE p@(A.Operator {A.args}) c = p { A.args = map (flip substE c) args }
substE p@(A.Function {A.args}) c = p { A.args = map (flip substE c) args }
substE p@(A.VAR {A.colName}) c   = case lookup colName c of
                                   Nothing -> p
                                   Just x  -> p {A.colName = x}
substE p@(A.VARSUB {A.colName}) c   = case lookup colName c of
                                    Nothing -> p
                                    Just x  -> p {A.colName = x}
substE p@(A.AGG {A.args}) c = p { A.args = map (flip substE c) args }
substE p@(A.BoolOp {A.args}) c = p { A.args = map (flip substE c) args }
substE p@(A.NullTest {A.arg}) c = p { A.arg = substE arg c }

substE e _ = e