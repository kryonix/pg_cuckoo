{-|
Module      : Validate
Description : This module checks whether a given input AST is valid or not
Author      : Denis Hirn

This module does a traversal of the given AST.
Errors are collected and can then be used for error messages.
-}

{-# LANGUAGE NamedFieldPuns #-}

module Validate ( validatePlannedStmt
                , validateOperator
                , validateExpr
                , isValidPlannedStmt
                , isValidOperator
                , isValidExpr ) where

import OperSem
import InAST as I

import Data.Maybe

import qualified Text.Show.Pretty as PP

data Log = Log { errors :: [ String ] }

instance Monoid Log where
    mempty = Log []
    (Log e1) `mappend` (Log e2) = Log (e1++e2)

logError :: Rule String ()
logError err = tell (Log [err])

type Rule a b = a -> OperSem () () b Log

-- | Checks whether an operator is valid or not
isValidPlannedStmt :: I.PlannedStmt -> Bool
isValidPlannedStmt op = null $ validatePlannedStmt op

-- | Validates an operator and returns all errors
validatePlannedStmt :: I.PlannedStmt -> [String]
validatePlannedStmt op = let
                      (_, lg) = runOperSem ((+>) op) () ()
                      (Log lg') = lg
                      in lg'

-- | Checks whether an operator is valid or not
isValidOperator :: I.Operator -> Bool
isValidOperator op = null $ validateOperator op 

-- | Validates an operator and returns all errors
validateOperator :: I.Operator -> [String]
validateOperator op = let
                (_, lg) = runOperSem ((~>) op) () ()
                (Log lg') = lg
                in lg'

-- | Checks whether an expression is valid or not
isValidExpr :: I.Expr -> Bool
isValidExpr op = null $ validateExpr op

-- | Validates an expression and returns all errors
validateExpr :: I.Expr -> [String]
validateExpr op = let
                (_, lg) = runOperSem ((~~>) op) () ()
                (Log lg') = lg
                in lg'

-- | Validate PlannedStmt
(+>) :: Rule I.PlannedStmt ()
(+>) (PlannedStmt {I.planTree, I.subplans})
  = do
    (~>) planTree
    mapM_ (~>) subplans

-- | Operator validator
(~>) :: Rule I.Operator ()
(~>) (SEQSCAN { targetlist, qual, scanrelation })
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual
    when (null scanrelation) 
      $ logError $ "SEQSCAN error: scanrelation is empty"

(~>) (RESULT { targetlist, resconstantqual })
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~~>) resconstantqual

(~>) (PROJECTSET { targetlist, operator })
  = do
    mapM_ (~~~>) targetlist
    (~>) operator

(~>) (LIMIT { operator, limitOffset, limitCount })
  = do
    (~>) operator
    mapM_ (~~>) limitOffset
    mapM_ (~~>) limitCount
    when (isNothing limitOffset && isNothing limitCount)
      $ logError $ "LIMIT: neither limitOffset nor limitCount specified"

(~>) (SORT {targetlist, operator, sortCols})
  = do
    when (null sortCols)
      $ logError $ "SORT: no sortCols specified"
    mapM_ (~~~>) targetlist
    (~>) operator

(~>) (GROUP {targetlist, qual, operator, groupCols})
  = do
    when (null groupCols)
      $ logError $ "GROUP: no groupCols specified"
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual
    (~>) operator

(~>) (APPEND {targetlist, appendplans})
  = do
    mapM_ (~~~>) targetlist
    when (null appendplans)
      $ logError $ "APPEND: no appendplans specified"
    mapM_ (~>) appendplans

(~>) (MERGEAPPEND { targetlist, mergeplans, sortCols })
  = do
    when (null mergeplans)
      $ logError $ "MERGEAPPEND: no mergeplans specified"
    when (null sortCols)
      $ logError $ "MERGEAPPEND: no sortCols specified"
    mapM_ (~~~>) targetlist
    mapM_ (~>) mergeplans

(~>) (RECURSIVEUNION {targetlist, lefttree, righttree, ctename})
  = do
    when (null ctename)
      $ logError $ "RECURSIVEUNION: ctename not specified"
    mapM_ (~~~>) targetlist
    (~>) lefttree
    (~>) righttree

(~>) (WORKTABLESCAN {targetlist, qual})
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual

(~>) (BITMAPAND {bitmapplans})
  = do
    when (null bitmapplans)
      $ logError $ "BITMAPAND: no bitmapplans specified"
    mapM_ (~>) bitmapplans

(~>) (BITMAPOR {bitmapplans})
  = do
    when (null bitmapplans)
      $ logError $ "BITMAPOR: no bitmapplans specified"
    mapM_ (~>) bitmapplans

(~>) (INDEXSCAN {targetlist, qual, indexqual, indexname, scanrelation})
  = do
    when (null indexname)
      $ logError "INDEXSCAN: indexname not specified"

    when (null scanrelation)
      $ logError $ "INDEXSCAN error: scanrelation is empty"

    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual
    mapM_ (~~>) indexqual

(~>) (INDEXONLYSCAN {targetlist, qual, indexqual, indexname, scanrelation})
  = do
    when (null indexname)
      $ logError "INDEXONLYSCAN: indexname not specified"

    when (null scanrelation)
      $ logError $ "INDEXONLYSCAN error: scanrelation is empty"

    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual
    mapM_ (~~>) indexqual

(~>) (BITMAPINDEXSCAN {indexqual, indexname, scanrelation})
  = do
    when (null indexname)
      $ logError "BITMAPINDEXSCAN: indexname not specified"

    when (null scanrelation)
      $ logError $ "BITMAPINDEXSCAN error: scanrelation is empty"

    mapM_ (~~>) indexqual

(~>) (BITMAPHEAPSCAN {targetlist, bitmapqualorig, operator, scanrelation})
  = do
    when (null scanrelation)
      $ logError $ "BITMAPHEAPSCAN error: scanrelation is empty"

    mapM_ (~~~>) targetlist
    mapM_ (~~>) bitmapqualorig
    (~>) operator

(~>) (AGG {targetlist, operator})
  = do
    mapM_ (~~~>) targetlist
    (~>) operator

(~>) (WINDOWAGG {targetlist, operator, frameOptions, startOffset, endOffset})
  = do
    when (null frameOptions)
      $ logError "WiNDOWAGG error: no frameOptions specified"
    mapM_ (~~~>) targetlist
    (~>) operator
    mapM_ (~~>) startOffset
    mapM_ (~~>) endOffset

(~>) (MATERIAL {operator}) = (~>) operator

(~>) (NESTLOOP {targetlist, joinquals, nestParams, lefttree, righttree})
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~~>) joinquals

    (~>) lefttree
    (~>) righttree

(~>) (MERGEJOIN {targetlist, qual, joinquals, mergeclauses, mergeStrategies, lefttree, righttree})
  = do
    when (length mergeclauses /= length mergeStrategies)
      $ logError "MERGEJOIN: mergeStrategies must have same length as the mergeclauses list"
    mapM_ (~~~>) targetlist
    mapM_ (~~>) joinquals
    mapM_ (~~>) mergeclauses
    mapM_ (~~>) qual

    (~>) lefttree
    (~>) righttree

(~>) (UNIQUE {operator, uniqueCols})
  = do
    when (null uniqueCols)
      $ logError $ "UNIQUE: no uniqueCols specified"
    (~>) operator

(~>) (SUBQUERYSCAN {targetlist, qual, subplan})
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual
    (~>) subplan

(~>) (FUNCTIONSCAN { targetlist, qual, functions })
  = do
    when (null functions)
      $ logError $ "FUNCTIONSCAN: no functions specified"
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual

(~>) (VALUESSCAN {targetlist, qual, values_list})
  = do
    when (null values_list)
      $ logError $ "VALUESSCAN: no values specified"
    mapM_ (~~>) qual
    mapM_ (mapM_ (~~>)) values_list

(~>) (CTESCAN {targetlist, qual})
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual

(~>) (GATHER {targetlist, operator})
  = do
    mapM_ (~~~>) targetlist
    (~>) operator

(~>) (GATHERMERGE {targetlist, operator})
  = do
    mapM_ (~~~>) targetlist
    (~>) operator


(~>) (HASH {targetlist, qual, operator, skewTable})
  = do
    when (null skewTable)
      $ logError "HASH: no skewTable specified"
    mapM_ (~~>) qual
    mapM_ (~~~>) targetlist
    (~>) operator

(~>) (HASHJOIN {targetlist, joinquals, hashclauses, lefttree, righttree})
  = do
    when (null hashclauses)
      $ logError "HASHJOIN: no hashclauses specified"

    mapM_ (~~~>) targetlist
    mapM_ (~~>) joinquals
    mapM_ (~~>) hashclauses
    (~>) lefttree

    case righttree of
      (HASH {}) -> (~>) righttree
      _         -> do
                    logError
                      $ "HASHJOIN: righttree is not HASH"
                    (~>) righttree

(~>) (SETOP {targetlist, qual, lefttree})
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual

    (~>) lefttree

(~>) (PARALLEL {operator}) = (~>) operator

-- | TargetEntry validator
(~~~>) :: Rule I.TargetEntry ()
(~~~>) (TargetEntry { targetexpr, targetresname })
  = do
    (~~>) targetexpr
    when (null targetresname) $ logError $ "TargetEntry error: targetresname is empty"

-- | Expression validator
(~~>) :: Rule I.Expr ()
(~~>) (VAR { varTable, varColumn })
  = do
    when (null varTable) $ logError $ "VAR error: varTable is empty"
    when (null varColumn) $ logError $ "VAR error: varColumn is empty"

(~~>) (SCANVAR {colPos})
  = do
    when (colPos <= 0) $ logError $ "SCANVAR error: colPos invalid"

(~~>) (CONST { constvalue, consttype })
  = do
    when (null constvalue) $ logError $ "CONST error: constvalue is empty"
    when (null consttype) $ logError $ "CONST error: consttype is empty"

(~~>) (FUNCEXPR { funcname, funcargs })
  = do
    when (null funcname) $ logError $ "FUNCEXPR error: funcname is empty"
    mapM_ (~~>) funcargs

(~~>) o@(OPEXPR { oprname, oprargs })
  = do
    when (null oprname) $ logError $ "OPEXPR error: oprname is empty"
    when (null oprargs) $ logError $ "OPEXPR error: no arguments found"
                                    ++ "\n" ++ PP.ppShow o
    when (length oprargs > 2) 
      $ logError $ "OPEXPR error: expected one or two arguments but got " 
                  ++ show (length oprargs)
                  ++ "\n" ++ PP.ppShow o
    mapM_ (~~>) oprargs

(~~>) (WINDOWFUNC {winname, winargs, aggfilter})
  = do
    when (null winname) $ logError "WINDOWFUNC error: winname is empty"
    mapM_ (~~>) winargs
    mapM_ (~~>) aggfilter

(~~>) o@(AGGREF { aggname, aggargs, aggdirectargs, aggfilter })
  = do
    when (null aggname)
      $ logError $ "AGGREF error: aggname is empty"
    mapM_ (~~~>) aggargs
    mapM_ (~~>) aggdirectargs
    mapM_ (~~>) aggfilter

(~~>) (AND { args }) = mapM_ (~~>) args
(~~>) (OR { args }) = mapM_ (~~>) args
(~~>) (NOT { arg }) = (~~>) arg
