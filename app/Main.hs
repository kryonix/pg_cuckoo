module Main where

import System.Environment
import Data.ConfigFile
import Data.Either.Utils

import Lib as L
import Validate as V

import Parser

import Text.Show.Pretty as PP hiding (List, Value, Float)

import GetTable
import qualified InAST as A
import Inference as I
import Extract as E

import GPrint

const1 :: A.Operator
const1 = A.RESULT
          { A.targetlist = 
              [ A.TargetEntry
                { A.targetexpr = A.CONST "42" "int4"
                , A.targetresname = "valid"
                }
              ]
          , A.qual = Nothing
          }


-- access list elements safely
(!!) :: [a] -> Int -> Maybe a
(!!) lst idx = if idx >= length lst
                then Nothing
                else Just $ lst Prelude.!! idx

main :: IO ()
main = do
    cmdArgs <- getArgs
    let configFile =
            case cmdArgs Main.!! 0 of
                Just j -> j -- config.ini file
                Nothing -> error "please provide a config file"
    config <- readfile emptyCP configFile
    let cp = forceEither config
    let authStr = forceEither $ get cp "Main" "dbauth" :: String

    putStrLn $ "Validate const1:"
    let errs = V.validateOperator const1
    putStrLn $ PP.ppShow errs

    tableDataR <- getTableData authStr

    let consts = E.extract const1
    putStrLn $ PP.ppShow consts

    consts' <- mapM (\x -> L.parseConst authStr x >>= \p -> return (x, p)) consts

    putStrLn $ PP.ppShow consts'

    let infered = generatePlan tableDataR consts' const1

    putStrLn $ PP.ppShow infered

    putStrLn $ gprint infered
