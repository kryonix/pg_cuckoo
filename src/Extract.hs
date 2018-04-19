{-|
Module      : Extract
Description : Extracts nodes from the input AST
Author      : Denis Hirn

-}

module Extract ( extract ) where

import OperSem
import InAST as I

data Log = Log { consts :: [I.Expr] }

instance Monoid Log where
    mempty = Log []
    (Log e1) `mappend` (Log e2) = Log (e1++e2)

logConst :: Rule I.Expr ()
logConst expr = tell (Log [expr])

type Rule a b = a -> OperSem () () b Log

-- | Extracts nodes
extract :: I.Operator -> [I.Expr]
extract op = let
              (_, lg) = runOperSem ((~>) op) () ()
              (Log lg') = lg
              in lg'

-- | Operator validator
(~>) :: Rule I.Operator ()
(~>) (SEQSCAN { targetlist=targetlist
                    , qual=qual
                    , scanrelation=scanrelation
                    })
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual

(~>) (RESULT { targetlist=targetlist
             , qual=qual })
  = do
    mapM_ (~~~>) targetlist
    mapM_ (~~>) qual

-- | TargetEntry validator
(~~~>) :: Rule I.TargetEntry ()
(~~~>) (TargetEntry { targetexpr=targetexpr
                    , targetresname=targetresname
                    })
  = do
    (~~>) targetexpr

-- | Expression validator
(~~>) :: Rule I.Expr ()
(~~>) c@(CONST { constvalue=constvalue
             , consttype=consttype
             })
  = logConst c
