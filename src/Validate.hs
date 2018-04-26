{-|
Module      : Validate
Description : This module checks whether a given input AST is valid or not
Author      : Denis Hirn

This module does a traversal of the given AST.
Errors are collected and can then be used for error messages.
-}

{-# LANGUAGE NamedFieldPuns #-}

module Validate ( validateOperator
                , validateExpr
                , isValidOperator
                , isValidExpr ) where

import OperSem
import InAST as I

import qualified Text.Show.Pretty as PP

data Log = Log { errors :: [ String ] }

instance Monoid Log where
    mempty = Log []
    (Log e1) `mappend` (Log e2) = Log (e1++e2)

logError :: Rule String ()
logError err = tell (Log [err])

type Rule a b = a -> OperSem () () b Log

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

(~>) (LIMIT { operator, limitOffset, limitCount })
  = do
    (~>) operator
    mapM_ (~~>) limitOffset
    mapM_ (~~>) limitCount

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
