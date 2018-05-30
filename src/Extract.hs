{-|
Module      : Extract
Description : Extracts nodes from the input AST
Author      : Denis Hirn

-}

{-# LANGUAGE NamedFieldPuns #-}

module Extract ( extract, extractP, Log(..) ) where

import OperSem
import InAST as I

data Log = Log { lgconsts :: [I.Expr]
               , lgTableNames :: [String]
               , lgScan :: [I.Operator]
               }
  deriving(Show)

instance Monoid Log where
    mempty = Log [] [] []
    (Log e1 e2 e3)
      `mappend` (Log n1 n2 n3)
        = Log (e1++n1) (e2++n2) (e3++n3)

logConst :: Rule I.Expr ()
logConst expr = tell (Log [expr] [] [])

logTable :: Rule String ()
logTable tname = tell (Log [] [tname] [])

logScan :: Rule I.Operator ()
logScan scan = tell (Log [] [] [scan])

type Rule a b = a -> OperSem () () b Log

-- | Extracts nodes
extractP :: I.PlannedStmt -> Log
extractP op = let
              (_, lg) = runOperSem ((+>) op) () ()
              in lg

-- | Extracts nodes
extract :: I.Operator -> Log
extract op = let
              (_, lg) = runOperSem ((~>) op) () ()
              in lg

(+>) :: Rule I.PlannedStmt ()
(+>) (PlannedStmt {I.planTree, I.subplans})
  = do
    (~>) planTree
    mapM_ (~>) subplans

-- | Operator validator
(~>) :: Rule I.Operator ()
(~>) (SEQSCAN { targetlist=targetlist
              , qual=qual
              , scanrelation=scanrelation
              })
  = do
    logTable scanrelation
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual

(~>) (RESULT { targetlist
             , resconstantqual })
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~~>) resconstantqual

(~>) (PROJECTSET { targetlist, operator })
  = do
    mapM_ (~~~>) targetlist
    (~>) operator

(~>) (LIMIT { operator=operator
            , limitOffset=limitOffset
            , limitCount=limitCount})
  = do
    (~>) operator
    mapM_ (~~>) limitOffset
    mapM_ (~~>) limitCount

(~>) (SORT {targetlist, operator})
  = do
    mapM_ (~~~>) targetlist
    (~>) operator

(~>) (GROUP {targetlist, qual, operator})
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual
    (~>) operator

(~>) (APPEND {targetlist, appendplans})
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~>) appendplans

(~>) (MERGEAPPEND { targetlist, mergeplans })
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~>) mergeplans

(~>) (RECURSIVEUNION {targetlist, lefttree, righttree})
  = do
    mapM_ (~~~>) targetlist
    (~>) lefttree
    (~>) righttree

(~>) w@(WORKTABLESCAN {targetlist, qual})
  = do
    logScan w
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual

(~>) (BITMAPAND { bitmapplans }) = mapM_ (~>) bitmapplans
(~>) (BITMAPOR { bitmapplans }) = mapM_ (~>) bitmapplans

(~>) i@(INDEXSCAN {targetlist, qual, indexqual, indexname, scanrelation})
  = do
    -- logScan i
    logTable scanrelation
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual
    mapM_ (~~>) indexqual

(~>) i@(INDEXONLYSCAN {targetlist, qual, indexqual, indexname, scanrelation})
  = do
    -- logScan i
    logTable scanrelation
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual
    mapM_ (~~>) indexqual

(~>) (BITMAPINDEXSCAN {indexqual, indexname, scanrelation})
  = do
    logTable scanrelation
    mapM_ (~~>) indexqual

(~>) (BITMAPHEAPSCAN {targetlist, bitmapqualorig, operator, scanrelation})
  = do
    logTable scanrelation
    mapM_ (~~~>) targetlist
    mapM_ (~~>) bitmapqualorig
    (~>) operator


(~>) (AGG {targetlist, operator})
  = do
    mapM_ (~~~>) targetlist
    (~>) operator

(~>) (WINDOWAGG {targetlist, operator, frameOptions, startOffset, endOffset})
  = do
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

(~>) (MERGEJOIN {targetlist, qual, joinquals, mergeclauses, lefttree, righttree})
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~~>) joinquals
    mapM_ (~~>) mergeclauses
    mapM_ (~~>) qual

    (~>) lefttree
    (~>) righttree

(~>) (UNIQUE {operator}) = (~>) operator

(~>) s@(SUBQUERYSCAN {targetlist, qual, subplan})
  = do
    logScan s
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual
    (~>) subplan

(~>) f@(FUNCTIONSCAN { targetlist, qual, functions })
  = do
    logScan f
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual
    mapM_ (~~>) functions

(~>) s@(VALUESSCAN {targetlist, qual, values_list})
  = do
    logScan s
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual
    mapM_ (mapM_ (~~>)) values_list

(~>) s@(CTESCAN {targetlist, qual})
  = do
    logScan s
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
    logTable skewTable
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual
    (~>) operator

(~>) (HASHJOIN {targetlist, joinquals, hashclauses, lefttree, righttree})
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~~>) joinquals
    mapM_ (~~>) hashclauses
    (~>) lefttree
    (~>) righttree

(~>) (SETOP {targetlist, qual, lefttree})
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual

    (~>) lefttree

(~>) (PARALLEL {operator}) = (~>) operator

-- | TargetEntry extract
(~~~>) :: Rule I.TargetEntry ()
(~~~>) (I.TargetEntry { targetexpr }) = (~~>) targetexpr

-- | Expression validator
(~~>) :: Rule I.Expr ()
(~~>) c@(CONST { constvalue=constvalue
               , consttype=consttype
               })
  = logConst c

(~~>) (FUNCEXPR { funcargs })
  = mapM_ (~~>) funcargs

(~~>) (OPEXPR { oprargs })
  = mapM_ (~~>) oprargs

(~~>) (AGGREF { aggargs, aggdirectargs, aggfilter })
  = do
    mapM_ (~~~>) aggargs
    mapM_ (~~>) aggdirectargs
    mapM_ (~~>) aggfilter

(~~>) (WINDOWFUNC {winargs, aggfilter})
  = do
    mapM_ (~~>) winargs
    mapM_ (~~>) aggfilter

(~~>) (AND { args }) = mapM_ (~~>) args
(~~>) (OR { args }) = mapM_ (~~>) args
(~~>) (NOT { arg }) = (~~>) arg

(~~>) e = return ()