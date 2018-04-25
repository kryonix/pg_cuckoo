module Main where

import System.Environment
import Data.ConfigFile
import Data.Either.Utils

import Control.Monad

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
          , A.resconstantqual = Nothing
          }

-- Query would be: select a as "foo", b as "bar" from grp
seq1 :: A.Operator
seq1 = A.SEQSCAN
        { A.targetlist =
            [ A.TargetEntry
                { A.targetexpr = A.VAR {A.varTable="grp", A.varColumn="a"}
                , A.targetresname = "foo"
                }
            , A.TargetEntry 
                { A.targetexpr = A.VAR {A.varTable="grp", A.varColumn="b"}
                , A.targetresname = "bar"
                }
            ]
        , A.qual = []
        , A.scanrelation="grp"
        }

seq2 :: A.Operator
seq2 = A.LIMIT
        { A.operator = seq1
        , A.limitOffset = Nothing
        , A.limitCount  = Just (A.CONST "1" "int8")
        }

func1 :: A.Operator
func1 = A.RESULT
          { A.targetlist =
              [ A.TargetEntry
                { A.targetexpr = 
                    A.FUNCEXPR 
                      { A.funcname="int4pl"
                      , A.funcargs=
                          [ A.CONST "1" "int4"
                          , A.CONST "41" "int4"
                          ]
                      }
                , A.targetresname = "addition"
                }
              ]
          , A.resconstantqual = Nothing
          }

seq3 :: A.Operator
seq3 = A.SEQSCAN
        { A.targetlist =
            [ A.TargetEntry
                { A.targetexpr = A.VAR {A.varTable="grp", A.varColumn="a"}
                , A.targetresname = "foo"
                }
            , A.TargetEntry 
                { A.targetexpr = A.VAR {A.varTable="grp", A.varColumn="b"}
                , A.targetresname = "bar"
                }
            , A.TargetEntry
                { A.targetexpr =
                    A.FUNCEXPR
                      { A.funcname="int4pl"
                      , A.funcargs=
                          [ A.VAR {A.varTable="grp", A.varColumn="a"}
                          , A.VAR {A.varTable="grp", A.varColumn="b"}
                          ] 
                      }
                , A.targetresname = "baz"
                }
            ]
        , A.qual = []
        , A.scanrelation="grp"
        }

seq4 :: A.Operator
seq4 = A.SEQSCAN
        { A.targetlist =
            [ A.TargetEntry
                { A.targetexpr = A.VAR {A.varTable="grp", A.varColumn="a"}
                , A.targetresname = "foo"
                }
            , A.TargetEntry 
                { A.targetexpr = A.VAR {A.varTable="grp", A.varColumn="b"}
                , A.targetresname = "bar"
                }
            , A.TargetEntry
                { A.targetexpr =
                    A.FUNCEXPR
                      { A.funcname="cos"
                      , A.funcargs=
                          [ A.FUNCEXPR
                            { A.funcname="float8"
                            , A.funcargs=
                                [ A.FUNCEXPR
                                  { A.funcname="int4pl"
                                  , A.funcargs=
                                      [ A.VAR {A.varTable="grp", A.varColumn="a"}
                                      --, A.VAR {A.varTable="grp", A.varColumn="b"}
                                      ] 
                                  }
                                ]
                            }
                          ]
                      }
                , A.targetresname = "baz"
                }
            ]
        , A.qual = []
        , A.scanrelation="grp"
        }

seq5 :: A.Operator
seq5 = A.SEQSCAN
        { A.targetlist =
            [ A.TargetEntry
                { A.targetexpr = A.VAR {A.varTable="grp", A.varColumn="a"}
                , A.targetresname = "foo"
                }
            , A.TargetEntry 
                { A.targetexpr = A.VAR {A.varTable="grp", A.varColumn="b"}
                , A.targetresname = "bar"
                }
            ]
        , A.qual =
            [ A.FUNCEXPR 
                { A.funcname = "int4lt"
                , A.funcargs =
                    [ A.VAR {A.varTable="grp", A.varColumn="a"}
                    , A.CONST "2" "int4"
                    ]
                }
            ]
        , A.scanrelation="grp"
        }

-- access list elements safely
(!!) :: [a] -> Int -> Maybe a
(!!) lst idx = if idx >= length lst
                then Nothing
                else Just $ lst Prelude.!! idx

checkAndGenerate :: String -> A.Operator -> IO ()
checkAndGenerate authStr op = do
  -- Validate the AST
  putStrLn $ "Validate: "
  let errs = V.validateOperator op
  -- Print errors
  putStrLn $ PP.ppShow errs

  unless (null errs) $ error $ "AST could not be validated:\n" ++ PP.ppShow errs

  -- Get Catalog data
  tableDataR <- getTableData authStr

  -- Use Extract.hs to extract information from AST to be pre-transformed etc.
  let consts = E.extract op
  putStrLn $ PP.ppShow consts

  -- Compile constants
  consts' <- mapM (\x -> L.parseConst authStr x >>= \p -> return (x, p)) $ lgconsts consts
  
  -- Debug output of constants
  putStrLn $ PP.ppShow consts'

  -- Infere output AST
  let infered = generatePlan tableDataR consts' (lgTableNames consts) op
  
  -- Print AST structure as well as the postgres plan
  putStrLn $ PP.ppShow infered
  putStrLn $ gprint infered

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

    checkAndGenerate authStr seq5
