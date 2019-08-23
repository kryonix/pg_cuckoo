{-|
Module      : TrChunks
Description : Translate constant SQL Values to Haskell ADTs
Copyright   : © Denis Hirn <denis.hirn@uni-tuebingen.de>
License     : AllRightsReserved
Maintainer  : Denis Hirn
-}

{-# LANGUAGE RecordWildCards #-}

module Database.PgCuckoo.TrChunks ( translate ) where

import qualified Data.Map as M
import Data.Maybe
import Database.PgCuckoo.PgPlan as O
import Database.PgCuckoo.Reader as R
import Text.Show.Pretty hiding (Float, List, Value)


(!) :: (Show a, Ord a) => M.Map a b -> a -> b
fs ! key = fromMaybe
            (error $ "given key " ++ ppShow key ++ " is not an element in the map")
            (M.lookup key fs)

translate :: Chunk -> O.Expr
translate (Chunk "CONST" fs) = O.CONST {..}
  where
    (Int consttype) = fs ! "consttype"
    (Int consttypmod) = fs ! "consttypmod"
    (Int constcollid) = fs ! "constcollid"
    (Int constlen) = fs ! "constlen"
    (Bool constbyval') = fs ! "constbyval"
    (Bool constisnull') = fs ! "constisnull"
    location = -1
    constbyval = pgConvert constbyval'
    constisnull = pgConvert constisnull'
    constvalue =
      case fs ! "constvalue" of
        (Extra (Int l) (Sequence constvalue')) -> Just $ Seq l constvalue'
        NoValue -> Nothing

translate err = error $ "Not implemented: " ++ ppShow err