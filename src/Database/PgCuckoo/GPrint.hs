{-|
Module      : GPrint
Description : Generic printer implementation
Copyright   : © Denis Hirn <denis.hirn@uni-tuebingen.de>
License     : AllRightsReserved
Maintainer  : Denis Hirn

This is the place where magic happens ¯\_(ツ)_/¯
-}

{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Database.PgCuckoo.GPrint
    ( GPrint(..)
    , GPrint1(..)
    ) where

import GHC.Generics
import Data.List
import Data.String.Utils

-- | Implementation of a generic record printer
-- With these classes and instances we just have to define
-- our records properly and derive {Generic, GPrint}.
-- The generic method gprint will then create the query-tree
-- format automagically.
defaultgprint :: (Generic a, GPrint1 (Rep a)) => a -> String
defaultgprint = gprint1 . from

class GPrint a where
  gprint :: a -> String
  default gprint :: (Generic a, GPrint1 (Rep a)) => a -> String
  gprint = defaultgprint

class GPrint1 a where
  gprint1 :: a p -> String

instance (GPrint a) => GPrint [a] where
  gprint [] = "<>"
  gprint xs = "(" ++ (intercalate " " $ map gprint xs) ++ ")"

instance (GPrint a) => GPrint (Maybe a) where
  gprint Nothing = "<>"
  gprint (Just x) = gprint x

instance {-# OVERLAPS #-} GPrint String where
  gprint x = replace " " "\\ " x

instance GPrint Integer where
  gprint x = show x

instance GPrint Double where
  gprint x = show x

instance GPrint Bool where
  gprint True = "true"
  gprint False = "false"

-- | Fallback instance when no GPrint1 instance is defined
instance {-# OVERLAPPABLE #-} (GPrint a) => GPrint1 (K1 i a) where
  gprint1 (K1 x) = gprint x

-- DataType name, we're going to ignore this and just pass through
instance (GPrint1 f, Datatype c) => GPrint1 (M1 D c f) where
  gprint1 m@(M1 x) = gprint1 x

-- Constructor name, we want to use this as the node type
instance (GPrint1 f, Constructor c) => GPrint1 (M1 C c f) where
  gprint1 m@(M1 x) = case conName m of
                        "GenericPlan" -> gprint1 x
                        "GenericRangeExPre" -> gprint1 x
                        "GenericRangeExPost" -> gprint1 x
                        c -> "{" ++ takeWhile (/= '_') c ++ " " ++ gprint1 x ++ "}"

-- Selector name, those are gonna be our fields
instance (GPrint1 f, Selector s) => GPrint1 (M1 S s f) where
  gprint1 m@(M1 x) = sel ++ gprint1 x
    where
      sel = case selName m of
              "" -> ""
              "genericPlan" -> ""
              "genericRangeExPre" -> ""
              "genericRangeExPost" -> ""
              e  -> ":" ++ dropWhile (== '_') e ++ " "

instance (GPrint1 a, GPrint1 b) => GPrint1 (a :*: b) where
  gprint1 (a :*: b) = gprint1 a ++ " " ++ gprint1 b

instance (GPrint1 a, GPrint1 b) => GPrint1 (a :+: b) where
  gprint1 (L1 x) = gprint1 x
  gprint1 (R1 x) = gprint1 x
