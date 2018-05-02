{-|
Module      : InAST
Description : Defines the input AST structure
Author      : Denis Hirn

-}

module InAST ( Operator(..)
             , TargetEntry(..)
             , SortEx(..)
             , Expr(..) ) where

data Operator = SEQSCAN
                { targetlist   :: [TargetEntry]
                , qual         :: [Expr]
                , scanrelation :: String
                }
              | RESULT
                { targetlist      :: [TargetEntry]
                , resconstantqual :: Maybe Expr
                }
              | LIMIT
                { operator :: Operator
                , limitOffset :: Maybe Expr
                , limitCount  :: Maybe Expr
                }
              | SORT
                { targetlist :: [TargetEntry]
                , operator   :: Operator
                , sortCols   :: [SortEx]
                }
              | APPEND
                { targetlist :: [TargetEntry]
                , appendplans :: [Operator]
                }
    deriving(Eq, Show)


data TargetEntry = TargetEntry
                    { targetexpr    :: Expr
                    , targetresname :: String
                    }
    deriving(Eq, Show)

data SortEx = SortEx
              { sortTarget     :: Integer
              , sortASC        :: Bool
              , sortNullsFirst :: Bool
              }
    deriving(Eq, Show)

data Expr = VAR
            { varTable  :: String
            , varColumn :: String
            }
          | CONST
            { constvalue :: String
            , consttype  :: String
            }
          | FUNCEXPR
            { funcname :: String
            , funcargs :: [Expr]
            }
          | OPEXPR
            { oprname :: String
            , oprargs :: [Expr]
            }
    deriving (Eq, Show)
