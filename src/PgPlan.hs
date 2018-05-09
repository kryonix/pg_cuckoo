{-|
Module      : PgPlan
Description : This module defines the data structures postgres expects (pretty printed)
Author      : Denis Hirn

The defined data types relate 1:1 to postgres plan nodes.
See: src/postgres/include/nodes/plannodes.h
     src/postgres/include/nodes/parsenodes.h
     src/postgres/include/nodes/primnodes.h
-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module PgPlan ( Null(..)
              , PLANNEDSTMT(..)
              , defaultPlannedStmt
              , GenericPlan(..)
              , defaultPlan
              , Plan(..)
              , Expr(..)
              , Seq(..)
              , TARGETENTRY(..)
              , RelationList(..)
              , Bitmapset(..)
              , Alias(..)
              , PlainList(..)
              , RangeEx(..)
              , List(..)
              , PgBool(..)
              , SORTGROUPCLAUSE(..)
              , NestLoopParam(..)
              ) where

import GPrint
import GHC.Generics
import Data.List

--------------------------------------------------------------------------------
-- BASE DATA TYPES

-- | Null data-type
-- Render this as '<>'
data Null = Null
    deriving (Eq, Show)

data List a = List [a]
    deriving (Eq, Show)

data PgBool = PgBool Bool
    deriving (Eq, Show)

data RelationList = RelationList [Integer]
    deriving (Eq, Show)

data Bitmapset = Bitmapset [Integer]
    deriving (Eq, Show)

-- | The same as List but gprint without parens
data PlainList a = PlainList [a]
    deriving(Eq, Show)

data Seq = Seq 
            { seqlength :: Integer
            , seqvalues :: [Integer] }
    deriving (Eq, Show)


pgFalse :: PgBool
pgFalse = PgBool False

pgTrue :: PgBool
pgTrue = PgBool True
-- / BASE DATA TYPES
--------------------------------------------------------------------------------
-- GPrint instances for base types

instance (GPrint a) => GPrint (List a) where
  gprint (List []) = "<>"
  gprint (List xs) = "(" ++ (intercalate " " $ map gprint xs) ++ ")"

instance (GPrint a) => GPrint (PlainList a) where
  gprint (PlainList []) = "" -- "<>"
  gprint (PlainList xs) = (intercalate " " $ map gprint xs)

instance GPrint Bitmapset where
  gprint (Bitmapset []) = "(b)"
  gprint (Bitmapset xs) = "(b " ++ (intercalate " " $ map gprint xs) ++ ")"

instance GPrint RelationList where
  gprint (RelationList []) = "<>"
  gprint (RelationList xs) = "(o " ++ (intercalate " " $ map gprint xs) ++ ")"

instance GPrint PgBool where
  gprint (PgBool x) = gprint x

instance GPrint Null where
  gprint Null = "<>"

instance GPrint Seq where
  gprint (Seq l v) = show l ++ " [ " ++ intercalate " " (map show v) ++ " ]"

instance GPrint Alias where
  gprint (Alias {aliasname=aliasname, colnames=List colnames})
    = "{ALIAS :aliasname " ++ aliasname ++ " :colnames (" ++ intercalate " " (map show colnames) ++ ")}"

-- / GPrint instances for base types
--------------------------------------------------------------------------------
-- Complex data types

data PLANNEDSTMT = PLANNEDSTMT
                    { commandType            :: Integer   -- Constant 1 (SELECT)
                    , queryId                :: Integer   -- Constant 0, doesn't matter
                    , hasReturning           :: PgBool      -- is it insert|update|delete RETURNING?
                    , hasModifyingCTE        :: PgBool      -- has insert|update|delete in WITH?
                    , canSetTag              :: PgBool      -- do I set the command result tag?
                    , transientPlan          :: PgBool      -- redo plan when TransactionXmin changes?
                    , dependsOnRole          :: PgBool      -- is plan specific to current role?
                    , parallelModeNeeded     :: PgBool      -- parallel mode required to execute?
                    , planTree               :: Plan      -- tree of Plan nodes
                    , rtable                 :: List RangeEx -- list of RangeTblEntry nodes
                    , resultRelations        :: Null      -- rtable indexes of target relations for INSERT/UPDATE/DELETE
                    , nonleafResultRelations :: Null      -- rtable indexes of non-leaf target relations for UPDATE/DELETE
                    , rootResultRelations    :: Null      -- rtable indexes of root target relations for UPDATE/DELETE
                    , subplans               :: List Plan    -- Plan trees for SubPlan expressions
                    , rewindPlanIDs          :: Bitmapset -- indices of subplans that require REWIND
                    , rowMarks               :: Null      -- a list of PlanRowMark's
                    , relationOids           :: RelationList -- OIDs of relations the plan depends on
                    , invalItems             :: Null      -- other dependencies, as PlanInvalItems
                    , nParamExec             :: Integer   -- type OIDs for PARAM_EXEC Params
                    , utilityStmt            :: Null      -- non-null if this is utility stmt
                    , stmt_location          :: Integer   -- start location, or -1 if unknown
                    , stmt_len               :: Integer   -- length in bytes; 0 means "rest of string"
                    }
    deriving (Eq, Show, Generic, GPrint)

defaultPlannedStmt :: PLANNEDSTMT
defaultPlannedStmt = PLANNEDSTMT
                      { commandType=1
                      , queryId=0
                      , hasReturning=pgFalse
                      , hasModifyingCTE=pgFalse
                      , canSetTag=pgTrue
                      , transientPlan=pgFalse
                      , dependsOnRole=pgFalse
                      , parallelModeNeeded=pgFalse
                      , planTree=RESULT defaultPlan Nothing
                      , rtable=List []
                      , resultRelations=Null
                      , nonleafResultRelations=Null
                      , rootResultRelations=Null
                      , subplans=List []
                      , rewindPlanIDs=Bitmapset []
                      , rowMarks=Null
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
                    , parallel_aware :: PgBool       -- engage parallel-aware logic?
                    , parallel_safe  :: PgBool       -- OK to use as part of parallel plan?
                    , plan_node_id   :: Integer    -- unique across entire final plan tree
                    , targetlist     :: List TARGETENTRY     -- target list to be computed at this node
                    , qual           :: List Expr -- implicitly-ANDed qual conditions
                    , lefttree       :: Maybe Plan -- input plan tree(s)
                    , righttree      :: Maybe Plan -- input
                    , initPlan       :: Maybe Plan -- Init Plan nodes (un-correlated expr subselects)
                    , extParam       :: Bitmapset  -- (b id₁ ... idₙ)
                    , allParam       :: Bitmapset  -- (b id₁ ... idₙ)
                    }
    deriving(Eq, Show, Generic, GPrint)

defaultPlan :: GenericPlan
defaultPlan = GenericPlan
              { startup_cost=0.0
              , total_cost=0.0
              , plan_rows=0
              , plan_width=0
              , parallel_aware=pgFalse
              , parallel_safe=pgFalse
              , plan_node_id=0
              , targetlist=List []
              , qual=List []
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

            Targetlist seems to be just duplicated from child.
            Use lefttree of genericPlan for child node.
        -}
          | LIMIT
            { genericPlan :: GenericPlan
            , limitOffset :: Maybe Expr  -- OFFSET parameter, or NULL if none 
            , limitCount  :: Maybe Expr  -- COUNT parameter, or NULL if none
            }
          | SORT
            { genericPlan   :: GenericPlan
            , numCols       :: Integer
            , sortColIdx    :: PlainList Integer
            , sortOperators :: PlainList Integer
            , collations    :: PlainList Integer
            , nullsFirst    :: PlainList PgBool
            }
          | APPEND
            { genericPlan :: GenericPlan
            , partitioned_rels :: Null
            , appendplans :: List Plan
            }
          | AGG
            { genericPlan  :: GenericPlan
            , aggstrategy  :: Integer           -- ^ basic strategy 0: AGG_PLAN, 1: AGG_SORTED, 2: AGG_HASHED, 3: AGG_MIXED
            , aggsplit     :: Integer           -- ^ agg-splitting mode
            , numCols      :: Integer           -- ^ number of grouping columns
            , grpColIdx    :: PlainList Integer -- ^ their indexes in the target list
            , grpOperators :: PlainList Integer -- ^ equality operators to compare with
            , numGroups    :: Integer           -- ^ estimated number of groups in input
            , aggParams    :: Bitmapset         -- ^ IDs of Params used in Aggref inputs
            , groupingSets :: Null              -- ^ grouping sets to use
            , chain        :: Null              -- ^ chained Agg/Sort nodes
            }
          | MATERIAL
            { genericPlan :: GenericPlan }
          | NESTLOOP
            { genericPlan :: GenericPlan
            , jointype    :: Integer
            , inner_unique :: PgBool
            , joinquals    :: List Expr
            , nestParams   :: List NestLoopParam
            }
          | UNIQUE
            { genericPlan :: GenericPlan
            , numCols     :: Integer
            , uniqColIdx  :: PlainList Integer
            , uniqOperators :: PlainList Integer
            }
    deriving (Eq, Show, Generic, GPrint)

data NestLoopParam = FIXME Null
    deriving (Eq, Show, Generic, GPrint)

data RangeEx = RTE
                { alias         :: Maybe Alias
                , eref          :: Alias
                , rtekind       :: Integer  -- Const 0
                , relid         :: Integer  -- OID of the relation
                , relkind       :: String   -- relation kind (see pg_class.relkind)
                , tablesample   :: Null
                , lateral       :: PgBool
                , inh           :: PgBool      -- Const false?
                , inFromCl      :: PgBool      -- Const true?
                , requiredPerms :: Integer   -- Const 2?
                , checkAsUser   :: Integer   -- Const 0?
                , selectedCols  :: Bitmapset
                , insertedCols  :: Bitmapset -- Const []
                , updatedCols   :: Bitmapset -- Const []
                , securityQuals :: Null
                }
    deriving (Eq, Show, Generic, GPrint)

data Alias = Alias
             { aliasname :: String
             , colnames  :: List String }
    deriving (Eq, Show, Generic)


{-TARGETENTRY         
:expr (...)          expression to evaluate
:resno 1             attribute number In a SELECT's targetlist,
                        * resno should always be equal to the 
                        * item's ordinal position (counting from 1)
:resname a           name of the column (could be NULL)
:ressortgroupref 0   nonzero if referenced by a sort/group clause
:resorigtbl ID       OID of column's source table
:resorigcol [1,n]    column's number in source table
:resjunk false       set to true to eliminate the attribute from final target list
-}

-- {resorigtbl, resorigcol} will be 0 if expr is e.g. a CONST (more cases to be determined)
data TARGETENTRY = TARGETENTRY
                    { expr            :: Expr
                    , resno           :: Integer
                    , resname         :: Maybe String
                    , ressortgroupref :: Integer
                    , resorigtbl      :: Integer
                    , resorigcol      :: Integer
                    , resjunk         :: PgBool
                    }
    deriving (Eq, Show, Generic, GPrint)


{-VAR              
:varno [1,n]          index of this var's relation in the range table, or INNER_VAR/OUTER_VAR/INDEX_VAR
:varattno [0,m]       attribute number of this var, or zero for all
:vartype 23       
:vartypmod -1     pg_attribute typmod value
:varcollid 0      OID of collation, or InvalidOid if none
:varlevelsup 0    for subquery variables referencing outer
                                 * relations; 0 in a normal var, >0 means N
                                 * levels up
:varnoold n       original value of varno, for debugging
:varoattno m      original value of varattno
:location -1      token location, or -1 if unknown
-}

data Expr = VAR
            { varno       :: Integer
            , varattno    :: Integer
            , vartype     :: Integer
            , vartypmod   :: Integer
            , varcollid   :: Integer
            , varlevelsup :: Integer
            , varnoold    :: Integer
            , varoattno   :: Integer
            , location    :: Integer
            }
          | CONST
            { consttype   :: Integer
            , consttypmod :: Integer
            , constcollid :: Integer
            , constlen    :: Integer
            , constbyval  :: PgBool
            , constisnull :: PgBool
            , location    :: Integer
            , constvalue  :: Maybe Seq
            }
  {-| FUNCEXPR
      COERCE_EXPLICIT_CALL,   /* display as a function call */
      COERCE_EXPLICIT_CAST,   /* display as an explicit cast */
      COERCE_IMPLICIT_CAST    /* implicit cast, so hide it */
  -}
          | FUNCEXPR
            { funcid         :: Integer    -- ^ PG_PROC OID of the function
            , funcresulttype :: Integer    -- ^ PG_TYPE OID of result value
            , funcretset     :: PgBool     -- ^ true if function returns set
            , funcvariadic   :: PgBool     -- ^ true if variadic arguments have been combined into an array last argument
            , funcformat     :: Integer    -- ^ how to display this function call
            , funccollid     :: Integer    -- ^ OID of collation of result
            , inputcollid    :: Integer    -- ^ OID of collation that function should use
            , args           :: List Expr  -- ^ arguments to the function
            , location       :: Integer
            }
          | OPEXPR
            { opno         :: Integer    -- ^ PG_OPERATOR OID of the operator
            , opfuncid     :: Integer    -- ^ PG_PROC OID of underlying function
            , opresulttype :: Integer    -- ^ PG_TYPE OID of result value
            , opretset     :: PgBool     -- ^ true if operator returns set
            , opcollid     :: Integer    -- ^ OID of collation of result
            , inputcollid  :: Integer    -- ^ OID of collation that operator should use
            , args         :: List Expr  -- ^ arguments to the operator (1 or 2)
            , location     :: Integer    -- ^ token location, or -1 if unknown
            }
          | AGGREF
            { aggfnoid      :: Integer              -- ^ pg_proc Oid of the aggregate
            , aggtype       :: Integer              -- ^ type Oid of result of the aggregate
            , aggcollid     :: Integer              -- ^ OID of collation of result
            , inputcollid   :: Integer              -- ^ OID of collation that function should use
            , aggtranstype  :: Integer              -- ^ type Oid of aggregate's transition value
            , aggargtypes   :: RelationList         -- ^ type Oids of direct and aggregated args
            , aggdirectargs :: List Expr            -- ^ direct arguments, if an ordered-set agg
            , _args         :: List TARGETENTRY     -- ^ aggregated arguments and sort expressions
            , aggorder      :: List SORTGROUPCLAUSE -- ^ ORDER BY
            , aggdistinct   :: List SORTGROUPCLAUSE -- ^ DISTINCT
            , aggfilter     :: Maybe Expr           -- ^ FILTER expression, if any
            , aggstar       :: PgBool               -- ^ TRUE if argument list was really '*'
            , aggvariadic   :: PgBool               -- ^ true if variadic arguments have been combined into an array last argument
            , aggkind       :: String               -- ^ aggregate kind (see pg_aggregate.h)
            , agglevelsup   :: Integer              -- ^ > 0 if agg belongs to outer query
            , _aggsplit      :: Integer              -- ^ expected agg-splitting mode of parent agg
            , location      :: Integer              -- ^ token location, or -1 if unknown
            }
    deriving (Eq, Show, Generic, GPrint)

data SORTGROUPCLAUSE = SORTGROUPCLAUSE
                       { tleSortGroupRef :: Integer
                       , eqop            :: Integer
                       , sortop          :: Integer
                       , nulls_first     :: PgBool
                       , hashable        :: PgBool
                       }
    deriving(Eq, Show, Generic, GPrint)

-- / Complex data types
