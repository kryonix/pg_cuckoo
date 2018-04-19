{-|
Module      : InAST
Description : Defines the input AST structure
Author      : Denis Hirn

-}

module InAST ( Operator(..)
             , TargetEntry(..)
             , Expr(..) ) where

data Operator = SEQSCAN
                { targetlist   :: [TargetEntry]
                , qual         :: Maybe Expr
                , scanrelation :: String
                }
              | RESULT
                { targetlist :: [TargetEntry]
                , qual       :: Maybe Expr
                }
    deriving(Eq, Show)


data TargetEntry = TargetEntry
                    { targetexpr    :: Expr
                    , targetresname :: String
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
    deriving (Eq, Show)