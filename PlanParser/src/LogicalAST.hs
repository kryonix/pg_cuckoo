{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : LogicalAST
Description : 
Copyright   : 2018 Denis Hirn
License     : AllRightsReserved
Maintainer  : Denis Hirn
-}

module LogicalAST ( Plan(..) -- , PlanF(..)
                  , TargetExpr(..)
                  , Expr(..)
                  , SortEx(..)
                  , JoinType(..)
                  , mapTargetExpr
                  , mapExpr
                  , mapPlan
                  ) where

import Control.Monad
import qualified Database.PgCuckoo.PgPlan as P

-- type Plan = PlanF Expr

-- data PlanF a = Projection
--             { targetList :: [ TargetExpr ]
--             , leftTree   :: PlanF a
--             }
--           | Selection
--             { predicate :: [a]
--             , leftTree  :: PlanF a
--             }
--           | Rename
--             { rename   :: [ (String, String) ]
--             , leftTree :: PlanF a
--             }
--           | Union
--             { leftTree :: PlanF a
--             , rightTree :: PlanF a
--             }
--           | Difference
--             { leftTree ::  PlanF a
--             , rightTree :: PlanF a
--             }
--           | Scan
--             { relation  :: String
--             , alias     :: String
--             , colnames  :: [ String ]
--             }
--           | Constant
--             { targetList :: [ TargetExpr ]
--             }
--           | Join
--             { leftTree   :: PlanF a
--             , rightTree  :: PlanF a
--             , predicate  :: [a]
--             , joinType   :: JoinType
--             }
--           | Sort
--             { sortCols :: [ SortEx ]
--             , leftTree :: PlanF a }
--           | DependentJoin
--             { leftTree :: PlanF a
--             , rightTree :: PlanF a
--             , predicate :: [a]
--             , binding   :: [(Integer, a)]
--             , joinType   :: JoinType
--             }
--           | GroupBy
--             { leftTree :: PlanF a
--             , groupCols :: [String] --[Integer]
--             , targetList :: [ TargetExpr ]
--             }
--           | Map
--             { leftTree :: PlanF a
--             , targetList :: [ TargetExpr ]
--             }
--           | Limit
--             { leftTree    :: PlanF a
--             , limitCount  :: Maybe a
--             , limitOffset :: Maybe a
--             }
--           | D { leftTree :: PlanF a }
--     deriving(Show, Eq, Functor, Foldable, Traversable, Generic)

data Plan = Projection
            { targetList :: [ TargetExpr ]
            , leftTree   :: Plan
            }
          | Selection
            { predicate :: [Expr]
            , leftTree  :: Plan
            }
          | Rename
            { rename   :: [ (String, String) ]
            , leftTree :: Plan
            }
          | Union
            { leftTree :: Plan
            , rightTree :: Plan
            }
          | Difference
            { leftTree ::  Plan
            , rightTree :: Plan
            }
          | Scan
            { relation  :: String
            , alias     :: String
            , colnames  :: [ String ]
            }
          | Constant
            { targetList :: [ TargetExpr ]
            }
          | Join
            { leftTree   :: Plan
            , rightTree  :: Plan
            , predicate  :: [Expr]
            , joinType   :: JoinType
            }
          | Sort
            { sortCols :: [ SortEx ]
            , leftTree :: Plan }
          | DependentJoin
            { leftTree :: Plan
            , rightTree :: Plan
            , predicate :: [Expr]
            , binding   :: [(Integer, Expr)]
            , joinType   :: JoinType
            }
          | GroupBy
            { leftTree :: Plan
            , groupCols :: [String] --[Integer]
            , targetList :: [ TargetExpr ]
            }
          | Map
            { leftTree :: Plan
            , targetList :: [ TargetExpr ]
            }
          | Limit
            { leftTree    :: Plan
            , limitCount  :: Maybe Expr
            , limitOffset :: Maybe Expr
            }
          | D { leftTree :: Plan }
    deriving(Show, Eq)

data TargetExpr = TargetExpr
                  { expr :: Expr
                  , resname :: Maybe String
                  , resno   :: Integer
                  }
    deriving(Show, Eq)

data Expr = Const
            { value :: String
            , typ  :: String }
          | Operator
            { operator :: String
            , args     :: [Expr]
            }
          | Function
            { function :: String
            , args     :: [Expr]
            }
          | DEP
            { arg :: Expr
            }
          | VAR
            { tableName :: String
            , colName   :: String
            }
          | VARSUB
            { colName :: String }
          | AGG
            { aggname :: String
            , args :: [Expr]
            , star :: Bool
            }
          | BoolOp
            { operator :: String
            , args     :: [Expr]
            }
          | NullTest
            { arg    :: Expr
            , isnull :: Bool
            }
          | Param
            { paramid :: Integer }
    deriving(Show, Eq)

data SortEx = SortEx
              { sortTarget     :: Expr
              , sortASC        :: Bool
              , sortNullsFirst :: Bool
              }
    deriving(Eq, Show)

data JoinType = INNER
              | LEFT
              | FULL
              | RIGHT
              | SEMI
              | ANTI
    deriving (Eq, Show)

mapTargetExpr :: Monad m => (Expr -> m Expr) -> TargetExpr -> m TargetExpr
mapTargetExpr f t@TargetExpr { expr }
  = do
    expr' <- f expr
    return $ t { expr = expr' }

mapArgs :: Monad m => (Expr -> m Expr) -> Expr -> m Expr
mapArgs f e = do
  e' <- mapM f (args e)
  return e { args = e' }

mapExpr :: Monad m => (Expr -> m Expr) -> Expr -> m Expr
mapExpr f e@Operator {} = mapArgs f e
mapExpr f e@Function {} = mapArgs f e
mapExpr f e@AGG {} = mapArgs f e
mapExpr f e@BoolOp {} = mapArgs f e
mapExpr f e@NullTest { arg }
  = do
    arg' <- f arg
    return $ e { arg = arg' }
mapExpr f e@DEP { arg }
  = do
    arg' <- f arg
    return $ e { arg = arg' }

mapExpr _ e = return e

mapLeft :: Monad m => (Plan -> m Plan) -> Plan -> m Plan
mapLeft f p = do
  p' <- f (leftTree p)
  return p { leftTree = p' }

mapRight :: Monad m => (Plan -> m Plan) -> Plan -> m Plan
mapRight f p = do
  p' <- f (rightTree p)
  return p { rightTree = p' }

mapPlan :: Monad m => (Plan -> m Plan) -> Plan -> m Plan
mapPlan f p@Projection {}    = mapLeft f p
mapPlan f p@Selection {}     = mapLeft f p
mapPlan f p@Rename {}        = mapLeft f p
mapPlan f p@Union {}         = (mapLeft f p >>= \x -> mapRight f x)
mapPlan f p@Difference {}    = (mapLeft f p >>= \x -> mapRight f x)
mapPlan f p@Join {}          = (mapLeft f p >>= \x -> mapRight f x)
mapPlan f p@Sort {}          = mapLeft f p
mapPlan f p@DependentJoin {} = (mapLeft f p >>= \x -> mapRight f x)
mapPlan f p@GroupBy {}       = mapLeft f p
mapPlan f p@Map {}           = mapLeft f p
mapPlan f p@Limit {}         = mapLeft f p
mapPlan f p@D {}             = mapLeft f p
mapPlan _ p = return p