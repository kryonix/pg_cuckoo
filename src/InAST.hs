{-|
Module      : InAST
Description : Defines the input AST structure
Author      : Denis Hirn

-}

module InAST ( Operator(..)
             , TargetEntry(..)
             , SortEx(..)
             , MergeEx(..)
             , Expr(..)
             , JoinType(..)
             , NestLoopParam(..) ) where

data Operator = SEQSCAN
                { targetlist   :: [TargetEntry]
                , qual         :: [Expr]
                , scanrelation :: String
                }
              | RESULT
                { targetlist      :: [TargetEntry]
                , resconstantqual :: Maybe Expr
                }
              | PROJECTSET
                { targetlist :: [TargetEntry]
                , operator   :: Operator
                }
              | LIMIT
                { operator    :: Operator
                , limitOffset :: Maybe Expr
                , limitCount  :: Maybe Expr
                }
              | SORT
                { targetlist :: [TargetEntry]
                , operator   :: Operator
                , sortCols   :: [SortEx]
                }
              | GROUP
                { targetlist :: [TargetEntry]
                , qual       :: [Expr]
                , operator   :: Operator
                , groupCols  :: [Integer]
                }
              | APPEND
                { targetlist  :: [TargetEntry]
                , appendplans :: [Operator]
                }
              | MERGEAPPEND
                { targetlist  :: [TargetEntry]
                , mergeplans :: [Operator]
                , sortCols    :: [SortEx]
                }
              | BITMAPAND
                { bitmapplans :: [Operator] }
              | BITMAPOR
                { bitmapplans :: [Operator] }
              | INDEXSCAN
                { targetlist    :: [TargetEntry]
                , qual          :: [Expr]
                , indexqual     :: [Expr]
                , indexorderby  :: [SortEx]
                , indexorderasc :: Bool
                , indexname     :: String
                , scanrelation  :: String
                }
              | INDEXONLYSCAN
                { targetlist    :: [TargetEntry]
                , qual          :: [Expr]
                , indexqual     :: [Expr]
                , indexorderby  :: [SortEx]
                , indexorderasc :: Bool
                , indexname     :: String
                , scanrelation  :: String
                }
              | BITMAPINDEXSCAN
                { indexqual    :: [Expr]
                , indexname    :: String
                , scanrelation :: String
                }
              | BITMAPHEAPSCAN
                { targetlist     :: [TargetEntry]
                , bitmapqualorig :: [Expr]
                , operator       :: Operator     -- Must be BITMAPINDEXSCAN?
                , scanrelation   :: String       -- might be inferable
                }
              | AGG
                { targetlist  :: [TargetEntry]
                , operator    :: Operator
                , groupCols   :: [Integer]
                }
              | MATERIAL
                { operator :: Operator }
              | NESTLOOP
                { targetlist   :: [TargetEntry]
                , joinType     :: JoinType      -- ^ rule for joining tuples from left and right subtrees
                , inner_unique :: Bool          -- ^ each outer tuple can match to no more than one inner tuple
                , joinquals    :: [Expr]        -- ^ qual conditions that came from JOIN/ON or JOIN/USING
                , nestParams   :: [NestLoopParam]
                , lefttree     :: Operator
                , righttree    :: Operator
                }
              | MERGEJOIN
                { targetlist      :: [TargetEntry]
                , qual            :: [Expr]
                , joinType        :: JoinType
                , inner_unique    :: Bool
                , joinquals       :: [Expr]
                , mergeclauses    :: [Expr]
                , mergeStrategies :: [MergeEx]
                , lefttree        :: Operator
                , righttree       :: Operator
                }
              | UNIQUE
                { operator   :: Operator
                , uniqueCols :: [Integer]
                }
              | FUNCTIONSCAN
                { targetlist     :: [TargetEntry]
                , qual           :: [Expr]
                , functions      :: [Expr]
                , funcordinality :: Bool
                }
              | VALUESSCAN
                { targetlist  :: [TargetEntry]
                , qual        :: [Expr]
                , values_list :: [[Expr]]
                }
              | HASH
                { targetlist :: [TargetEntry]
                , qual       :: [Expr]
                , operator   :: Operator
                , skewTable  :: String
                , skewColumn :: Integer
                }
              | HASHJOIN
                { targetlist   :: [TargetEntry]
                , joinType     :: JoinType
                , inner_unique :: Bool
                , joinquals    :: [Expr]
                , hashclauses  :: [Expr] -- OpExprs?
                , lefttree     :: Operator
                , righttree    :: Operator -- Must be HASH
                }
    deriving(Eq, Show)


{-
   nest loop join node

 The nestParams list identifies any executor Params that must be passed
 into execution of the inner subplan carrying values from the current row
 of the outer subplan.  Currently we restrict these values to be simple
 Vars, but perhaps someday that'd be worth relaxing.  (Note: during plan
 creation, the paramval can actually be a PlaceHolderVar expression; but it
 must be a Var with varno OUTER_VAR by the time it gets to the executor.)
-}

data NestLoopParam = NestLoopParam
                      { paramno  :: Integer
                      , paramval :: Expr
                      }
    deriving (Eq, Show)

data TargetEntry = TargetEntry
                    { targetexpr    :: Expr
                    , targetresname :: String
                    , resjunk       :: Bool
                    }
    deriving(Eq, Show)

data SortEx = SortEx
              { sortTarget     :: Integer
              , sortASC        :: Bool
              , sortNullsFirst :: Bool
              }
    deriving(Eq, Show)

data MergeEx = MergeEx
                { mergeASC        :: Bool
                , mergeNullsFirst :: Bool
                }
    deriving(Eq, Show)

data Expr = VAR
            { varTable  :: String
            , varColumn :: String
            }
          | SCANVAR
            { colPos :: Integer }
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
          | AGGREF
            { aggname       :: String
            , aggargs       :: [TargetEntry]
            , aggdirectargs :: [Expr]
            , aggorder      :: [SortEx]
            , aggdistinct   :: [SortEx]
            , aggfilter     :: Maybe Expr
            , aggstar       :: Bool
            }
          | AND { args :: [Expr] }
          | OR  { args :: [Expr] }
          | NOT { arg  :: Expr }
    deriving (Eq, Show)

data JoinType = INNER
              | LEFT
              | FULL
              | RIGHT
              | SEMI
              | ANTI
    deriving (Eq, Show)
