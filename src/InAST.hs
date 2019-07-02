{-|
Module      : InAST
Description : Defines the input AST structure
Copyright   : © Denis Hirn <denis.hirn@uni-tuebingen.de>
License     : AllRightsReserved
Maintainer  : Denis Hirn
-}

module InAST ( PlannedStmt(..)
             , Operator(..)
             , TargetEntry(..)
             , SortEx(..)
             , MergeEx(..)
             , SetOpCmd(..)
             , SetOpStrategy(..)
             , Expr(..)
             , JoinType(..)
             , NestLoopParam(..)
             , AggSplit(..)
             , AggStrategy(..)
             , aggSplitToInt
             , aggStrategyToInt
             , aggSPLIT_INITIAL_SERIAL
             , aggSPLIT_FINAL_DESERIAL
             , FrameOptions(..)
             , frameOptionToBits
             , SublinkType(..)
             , sublinkToInt
             , ParamKind(..)
             , paramKindToInt ) where

data PlannedStmt = PlannedStmt
                    { planTree :: Operator
                    , subplans :: [Operator]
                    }
    deriving (Eq, Show)

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
              | RECURSIVEUNION
                { targetlist :: [TargetEntry]
                , lefttree   :: Operator
                , righttree  :: Operator
                , wtParam    :: Integer
                , unionall   :: Bool
                , ctename    :: String
                }
              | WORKTABLESCAN
                { targetlist   :: [TargetEntry]
                , qual         :: [Expr]
                , wtParam      :: Integer
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
                , qual        :: [Expr]
                , operator    :: Operator
                , groupCols   :: [Integer]
                , aggstrategy :: AggStrategy
                , aggsplit    :: [AggSplit]
                -- HAVING IS MISSING HERE!!!
                }
              | WINDOWAGG
                { targetlist :: [TargetEntry]
                , operator   :: Operator
                , winrefId   :: Integer
                , ordEx      :: [SortEx]
                , groupCols  :: [Integer]
                , frameOptions :: [FrameOptions]
                , startOffset :: Maybe Expr
                , endOffset   :: Maybe Expr
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
              | SUBQUERYSCAN
                { targetlist :: [TargetEntry]
                , qual       :: [Expr]
                , subplan    :: Operator
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
              | CTESCAN
                { targetlist :: [TargetEntry]
                , qual       :: [Expr]
                , ctename    :: String
                , recursive  :: Bool
                , initPlan   :: [Integer]
                }
              | GATHER
                { targetlist   :: [TargetEntry]
                , num_workers  :: Integer
                , operator     :: Operator
                , rescan_param :: Integer
                }
              | GATHERMERGE
                { targetlist   :: [TargetEntry]
                , num_workers  :: Integer
                , operator     :: Operator
                , rescan_param :: Integer
                , sortCols     :: [SortEx]
                }
              | HASH
                { targetlist :: [TargetEntry]
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
              | SETOP
                { targetlist    :: [TargetEntry]
                , qual          :: [Expr]
                , setopStrategy :: SetOpStrategy
                , setOpCmd      :: SetOpCmd
                , lefttree      :: Operator
                , flagColIdx    :: Integer
                , firstFlag     :: Integer
                }
              {-
              NOT part of the algebra. Just sets the flags parallel_{safe, aware}
              to true.
              -}
              | PARALLEL
                { operator :: Operator }
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

data SetOpCmd = SETOPCMD_INTERSECT
              | SETOPCMD_INTERSECT_ALL
              | SETOPCMD_EXCEPT
              | SETOPCMD_EXCEPT_ALL
    deriving(Eq, Show)

data SetOpStrategy = SETOP_SORTED | SETOP_HASHED
    deriving(Eq, Show)

data Expr = VAR
            { varTable  :: String
            , varColumn :: String
            }
          | VARPOS
            { varTable :: String
            , varPos   :: Integer
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
          | WINDOWFUNC
            { winname   :: String
            , winargs   :: [Expr]
            , aggfilter :: Maybe Expr
            , winref    :: Integer
            , winstar   :: Bool
          }
          | AND { args :: [Expr] }
          | OR  { args :: [Expr] }
          | NOT { arg  :: Expr }
          | SUBPLAN
            { sublinkType :: SublinkType
            , testExpr    :: Maybe Expr
            , paramIds    :: [Integer]
            , plan_id     :: Integer
            , plan_name   :: String
            , firstColType :: String
            , setParam    :: [Integer]
            , parParam    :: [Integer]
            , args        :: [Expr]
            }
          | PARAM
            { paramkind :: ParamKind
            , paramid   :: Integer
            , paramtype :: String
            }
    deriving (Eq, Show)

data JoinType = INNER
              | LEFT
              | FULL
              | RIGHT
              | SEMI
              | ANTI
    deriving (Eq, Show)

data ParamKind = PARAM_EXTERN
               | PARAM_EXEC
               | PARAM_SUBLINK
               | PARAM_MULTIEXPR
    deriving(Eq, Show)

paramKindToInt :: ParamKind -> Integer
paramKindToInt PARAM_EXTERN    = 0
paramKindToInt PARAM_EXEC      = 1
paramKindToInt PARAM_SUBLINK   = 2
paramKindToInt PARAM_MULTIEXPR = 3

data SublinkType = EXISTS_SUBLINK
                 | ALL_SUBLINK
                 | ANY_SUBLINK
                 | ROWCOMPARE_SUBLINK
                 | EXPR_SUBLINK
                 | MULTIEXPR_SUBLINK
                 | ARRAY_SUBLINK
                 | CTE_SUBLINK
    deriving(Eq, Show)

sublinkToInt :: SublinkType -> Integer
sublinkToInt EXISTS_SUBLINK     = 0
sublinkToInt ALL_SUBLINK        = 1
sublinkToInt ANY_SUBLINK        = 2
sublinkToInt ROWCOMPARE_SUBLINK = 3
sublinkToInt EXPR_SUBLINK       = 4
sublinkToInt MULTIEXPR_SUBLINK  = 5
sublinkToInt ARRAY_SUBLINK      = 6
sublinkToInt CTE_SUBLINK        = 7

data AggStrategy = AGG_PLAIN
                 | AGG_SORTED
                 | AGG_HASHED
                 | AGG_MIXED
    deriving(Eq, Show)

data AggSplit = AGGSPLITOP_SIMPLE
              | AGGSPLITOP_COMBINE
              | AGGSPLITOP_SKIPFINAL
              | AGGSPLITOP_SERIALIZE
              | AGGSPLITOP_DESERIALIZE
    deriving(Eq, Show)

aggSPLIT_INITIAL_SERIAL :: [AggSplit]
aggSPLIT_INITIAL_SERIAL = [AGGSPLITOP_SKIPFINAL, AGGSPLITOP_SERIALIZE]

aggSPLIT_FINAL_DESERIAL :: [AggSplit]
aggSPLIT_FINAL_DESERIAL = [AGGSPLITOP_COMBINE, AGGSPLITOP_DESERIALIZE]

aggStrategyToInt :: AggStrategy -> Integer
aggStrategyToInt AGG_PLAIN  = 0
aggStrategyToInt AGG_SORTED = 1
aggStrategyToInt AGG_HASHED = 2
aggStrategyToInt AGG_MIXED  = 3

aggSplitToInt :: AggSplit -> Integer
aggSplitToInt AGGSPLITOP_SIMPLE      = 0x0
aggSplitToInt AGGSPLITOP_COMBINE     = 0x1
aggSplitToInt AGGSPLITOP_SKIPFINAL   = 0x2
aggSplitToInt AGGSPLITOP_SERIALIZE   = 0x4
aggSplitToInt AGGSPLITOP_DESERIALIZE = 0x8

data FrameOptions = FRAMEOPTION_NONDEFAULT
                  | FRAMEOPTION_RANGE
                  | FRAMEOPTION_ROWS
                  | FRAMEOPTION_BETWEEN
                  | FRAMEOPTION_START_UNBOUNDED_PRECEDING
                  | FRAMEOPTION_END_UNBOUNDED_PRECEDING
                  | FRAMEOPTION_START_UNBOUNDED_FOLLOWING
                  | FRAMEOPTION_END_UNBOUNDED_FOLLOWING
                  | FRAMEOPTION_START_CURRENT_ROW
                  | FRAMEOPTION_END_CURRENT_ROW
                  | FRAMEOPTION_START_VALUE_PRECEDING
                  | FRAMEOPTION_END_VALUE_PRECEDING
                  | FRAMEOPTION_START_VALUE_FOLLOWING
                  | FRAMEOPTION_END_VALUE_FOLLOWING
    deriving (Eq, Show)

frameOptionToBits :: FrameOptions -> Integer
frameOptionToBits FRAMEOPTION_NONDEFAULT                = 0x1
frameOptionToBits FRAMEOPTION_RANGE                     = 0x2
frameOptionToBits FRAMEOPTION_ROWS                      = 0x4
frameOptionToBits FRAMEOPTION_BETWEEN                   = 0x8
frameOptionToBits FRAMEOPTION_START_UNBOUNDED_PRECEDING = 0x10
frameOptionToBits FRAMEOPTION_END_UNBOUNDED_PRECEDING   = 0x20
frameOptionToBits FRAMEOPTION_START_UNBOUNDED_FOLLOWING = 0x40
frameOptionToBits FRAMEOPTION_END_UNBOUNDED_FOLLOWING   = 0x80
frameOptionToBits FRAMEOPTION_START_CURRENT_ROW         = 0x100
frameOptionToBits FRAMEOPTION_END_CURRENT_ROW           = 0x200
frameOptionToBits FRAMEOPTION_START_VALUE_PRECEDING     = 0x400
frameOptionToBits FRAMEOPTION_END_VALUE_PRECEDING       = 0x800
frameOptionToBits FRAMEOPTION_START_VALUE_FOLLOWING     = 0x1000
frameOptionToBits FRAMEOPTION_END_VALUE_FOLLOWING       = 0x2000
