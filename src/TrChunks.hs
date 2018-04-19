
module TrChunks ( translate ) where

import Data.Maybe
import Text.Show.Pretty hiding (List, Value, Float)
import qualified Data.Map as M
import Reader as R
import PgPlan as O

(!) :: (Show a, Ord a) => M.Map a b -> a -> b
fs ! key = fromMaybe
            (error $ "given key " ++ ppShow key ++ " is not an element in the map")
            (M.lookup key fs)

translate :: Chunk -> O.Expr
translate (Chunk "CONST" fs) 
        = O.CONST { consttype=consttype
                  , consttypmod=consttypmod
                  , constcollid=constcollid
                  , constlen=constlen
                  , constbyval=PgBool constbyval
                  , constisnull=PgBool constisnull
                  , location= -1
                  , constvalue=constvalue
                  }
    where
        (Int consttype) = fs ! "consttype"
        (Int consttypmod) = fs ! "consttypmod"
        (Int constcollid) = fs ! "constcollid"
        (Int constlen)    = fs ! "constlen"
        (Bool constbyval) = fs ! "constbyval"
        (Bool constisnull) = fs ! "constisnull"
        constvalue = case fs ! "constvalue" of
                        (Extra (Int l) (Sequence constvalue')) -> Just $ Seq l constvalue'
                        NoValue -> Nothing
