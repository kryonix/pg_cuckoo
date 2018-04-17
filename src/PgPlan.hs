{-|
Module      : PgPlan
Description : This module defines the data structures postgres expects (pretty printed)
Author      : Denis Hirn

The defined data types relate 1:1 to postgres plan nodes.
See: src/postgres/include/nodes/plannodes.h
     src/postgres/include/nodes/parsenodes.h
     src/postgres/include/nodes/primnodes.h
-}

{-# LANGUGAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}

module PgPlan ( Null
              , PlannedStmt
              , defaultPlannedStmt
              , GenericPlan
              , defaultPlan
              , Plan
              ) where

import Data.Data

-- | Null data-type
-- Render this as '<>'
data Null = Null
    deriving (Eq, Show)

data PlannedStmt = PlannedStmt
                    { commandType            :: Integer   -- Constant 1 (SELECT)
                    , queryId                :: Integer   -- Constant 0, doesn't matter
                    , hasReturning           :: Boolean   -- is it insert|update|delete RETURNING?
                    , hasModifyingCTE        :: Boolean   -- has insert|update|delete in WITH?
                    , canSetTag              :: Boolean   -- do I set the command result tag?
                    , transientPlan          :: Boolean   -- redo plan when TransactionXmin changes?
                    , dependsOnRole          :: Boolean   -- is plan specific to current role?
                    , parallelModeNeeded     :: Boolean   -- parallel mode required to execute?
                    , planTree               :: Plan      -- tree of Plan nodes
                    , rtable                 :: [RangeEx] -- list of RangeTblEntry nodes
                    , resultRelations        :: Null      -- rtable indexes of target relations for INSERT/UPDATE/DELETE
                    , nonleafResultRelations :: Null      -- rtable indexes of non-leaf target relations for UPDATE/DELETE
                    , rootResultRelations    :: Null      -- rtable indexes of root target relations for UPDATE/DELETE
                    , subplans               :: [Plan]    -- Plan trees for SubPlan expressions
                    , rewindPlanIDs          :: Bitmapset -- indices of subplans that require REWIND
                    , rowMarks               :: Null      -- a list of PlanRowMark's
                    , relationOids           :: RelationList -- OIDs of relations the plan depends on
                    , invalItems             :: Null      -- other dependencies, as PlanInvalItems
                    , nParamExec             :: Integer   -- type OIDs for PARAM_EXEC Params
                    , utilityStmt            :: Null      -- non-null if this is utility stmt
                    , stmt_location          :: Integer   -- start location, or -1 if unknown
                    , stmt_len               :: Integer   -- length in bytes; 0 means "rest of string"
                    }
    deriving (Eq, Show, Data, Typeable)

data RelationList = RelationList [Integer]
    deriving (Eq, Show)

defaultPlannedStmt :: PlannedStmt
defaultPlannedStmt = PlannedStmt
                      { commandType=1
                      , queryId=0
                      , hasReturning=False
                      , hasModifyingCTE=False
                      , canSetTag=True
                      , transientPlan=False
                      , dependsOnRole=False
                      , parallelModeNeeded=False
                      , planTree=defaultPlan
                      , rtable=[]
                      , resultRelations=Null
                      , nonleafResultRelations=Null
                      , rootResultRelations=Null
                      , subplans=[]
                      , rewindPlanIDs=Bitmapset []
                      , relationOids=RelationList []
                      , invalItems=Null
                      , nParamExec=0
                      , utilityStmt=Null
                      , stmt_location=0
                      , stmt_len=0
                      }

-- | Plan structure with all fields postgres needs
data GenericPlan = GenericPlan
                    { startup_cost   :: Double     -- cost expended before fetching any tuples
                    , total_cost     :: Double     -- total cost (assuming all tuples fetched)
                    , plan_rows      :: Integer    -- number of rows plan is expected to emit
                    , plan_width     :: Integer    -- average row width in bytes
                    , parallel_aware :: Boolean    -- engage parallel-aware logic?
                    , parallel_safe  :: Boolean    -- OK to use as part of parallel plan?
                    , plan_node_id   :: Integer    -- unique across entire final plan tree
                    , targetlist     :: [Expr]     -- target list to be computed at this node
                    , qual           :: Maybe Expr -- implicitly-ANDed qual conditions
                    , lefttree       :: Maybe Plan -- input plan tree(s)
                    , righttree      :: Maybe Plan -- input
                    , initPlan       :: Maybe Plan -- Init Plan nodes (un-correlated expr subselects)
                    , extParam       :: Bitmapset  -- (b id₁ ... idₙ)
                    , allParam       :: Bitmapset  -- (b id₁ ... idₙ)
                    }
    deriving(Eq, Show, Data, Typeable)

data Bitmapset = Bitmapset [Integer]
    deriving(Eq, Show)

defaultPlan :: GenericPlan
defaultPlan = GenericPlan
              { startup_cost=0.0
              , total_cost=0.0
              , plan_rows=0
              , plan_width=0
              , parallel_aware=False
              , parallel_safe=False
              , plan_node_id=0
              , targetlist=[]
              , qual=Nothing
              , lefttree=Nothing
              , righttree=Nothing
              , initPlan=Nothing
              , extParam=Bitmapset []
              , allParam=Bitmapset [] }

-- | Plan operators
{-| Result node -
      If no outer plan, evaluate a variable-free targetlist.
      If outer plan, return tuples from outer plan (after a level of
      projection as shown by targetlist).

      If resconstantqual isn't NULL, it represents a one-time qualification
      test (i.e., one that doesn't depend on any variables from the outer plan,
      so needs to be evaluated only once).
-}
data Plan = RESULT
            { genericPlan     :: GenericPlan
            , resconstantqual :: Maybe Expr }
        {-| ProjectSet node -
            Apply a projection that includes set-returning functions to the
            output tuples of the outer plan.
        -}
          | PROJECTSET
            { genericPlan :: GenericPlan }
          | SEQSCAN
            { genericPlan :: GenericPlan
            , scanrelid   :: Integer     -- relid is index into the range table
            }
        {-| limit node
            Note: as of Postgres 8.2, the offset and count expressions are expected
            to yield int8, rather than int4 as before.
        -}
          | LIMIT
            { genericPlan :: GenericPlan
            , limitOffset :: Maybe Expr  -- OFFSET parameter, or NULL if none 
            , limitCount  :: Maybe Expr  -- COUNT parameter, or NULL if none
            }
    deriving (Eq, Show, Data, Typeable)

data RangeEx = RTE
                { alias         :: Maybe Alias
                , eref          :: Alias
                , rtekind       :: Integer  -- Const 0
                , relid         :: Integer  -- OID of the relation
                , relkind       :: String   -- relation kind (see pg_class.relkind)
                , tablesample   :: Null
                , lateral       :: Boolean
                , inh           :: Boolean   -- Const false?
                , inFromCl      :: Boolean   -- Const true?
                , requiredPerms :: Integer   -- Const 2?
                , checkAsUser   :: Integer   -- Const 0?
                , selectedCols  :: Bitmapset
                , insertedCols  :: Bitmapset -- Const []
                , updatedCols   :: Bitmapset -- Const []
                , securityQuals :: Null
                }
    deriving (Eq, Show, Data, Typeable)

data Alias = Alias
             { aliasname :: String
             , colnames  :: [String] }
    deriving (Eq, Show, Data, Typeable)

-- How to implement this?
-- Use LogParser.AST? How can we infere all
-- information needed?
-- BIG problem!
data Expr = ???
