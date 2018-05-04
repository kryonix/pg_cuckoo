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
               }
  deriving(Show)

instance Monoid Log where
    mempty = Log [] []
    (Log e1 e2) `mappend` (Log n1 n2) = Log (e1++n1) (e2++n2)

logConst :: Rule I.Expr ()
logConst expr = tell (Log [expr] [])

logTable :: Rule String ()
logTable tname = tell (Log [] [tname])

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

(~>) (AGG {targetlist, operator})
  = do
    mapM_ (~~~>) targetlist
    (~>) operator

(~>) (APPEND {targetlist, appendplans})
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~>) appendplans

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