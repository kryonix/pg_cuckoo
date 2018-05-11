{-|
Module      : Extract
Description : Extracts nodes from the input AST
Author      : Denis Hirn

-}

{-# LANGUAGE NamedFieldPuns #-}

module Extract ( extract, Log(..) ) where

import OperSem
import InAST as I

data Log = Log { lgconsts :: [I.Expr]
               , lgTableNames :: [String]
               , lgValuesScan :: [I.Operator]
               }
  deriving(Show)

instance Monoid Log where
    mempty = Log [] [] []
    (Log e1 e2 e3) `mappend` (Log n1 n2 n3) = Log (e1++n1) (e2++n2) (e3++n3)

logConst :: Rule I.Expr ()
logConst expr = tell (Log [expr] [] [])

logTable :: Rule String ()
logTable tname = tell (Log [] [tname] [])

logValues :: Rule I.Operator ()
logValues scan = tell (Log [] [] [scan])

type Rule a b = a -> OperSem () () b Log

-- | Extracts nodes
extract :: I.Operator -> Log
extract op = let
              (_, lg) = runOperSem ((~>) op) () ()
              in lg

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

(~>) (APPEND {targetlist, appendplans})
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~>) appendplans

(~>) (AGG {targetlist, operator})
  = do
    mapM_ (~~~>) targetlist
    (~>) operator

(~>) (MATERIAL {operator}) = (~>) operator

(~>) (NESTLOOP {targetlist, joinquals, nestParams, lefttree, righttree})
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~~>) joinquals
    (~>) lefttree
    (~>) righttree

(~>) (UNIQUE {operator}) = (~>) operator

(~>) s@(VALUESSCAN {targetlist, qual, values_list})
  = do
    logValues s
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual
    mapM_ (mapM_ (~~>)) values_list

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

(~~>) e = return ()