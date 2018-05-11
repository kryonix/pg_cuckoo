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
import Data.List

const1 :: A.Operator
const1 = A.RESULT
          { A.targetlist = 
              [ A.TargetEntry
                { A.targetexpr = A.CONST "42" "int4"
                , A.targetresname = "valid"
                , A.resjunk = False
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
                , A.resjunk = False
                }
            , A.TargetEntry 
                { A.targetexpr = A.VAR {A.varTable="grp", A.varColumn="b"}
                , A.targetresname = "bar"
                , A.resjunk = False
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
                , A.resjunk = False
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
                , A.resjunk = False
                }
            , A.TargetEntry 
                { A.targetexpr = A.VAR {A.varTable="grp", A.varColumn="b"}
                , A.targetresname = "bar"
                , A.resjunk = False
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
                , A.resjunk = False
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
                , A.resjunk = False
                }
            , A.TargetEntry 
                { A.targetexpr = A.VAR {A.varTable="grp", A.varColumn="b"}
                , A.targetresname = "bar"
                , A.resjunk = False
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
                                      , A.VAR {A.varTable="grp", A.varColumn="b"}
                                      ] 
                                  }
                                ]
                            }
                          ]
                      }
                , A.targetresname = "baz"
                , A.resjunk = False
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
                , A.resjunk = False
                }
            , A.TargetEntry 
                { A.targetexpr = A.VAR {A.varTable="grp", A.varColumn="b"}
                , A.targetresname = "bar"
                , A.resjunk = False
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

func2 :: A.Operator
func2 = A.RESULT
          { A.targetlist =
              [ A.TargetEntry
                { A.targetexpr = 
                    A.OPEXPR
                      { A.oprname="<"
                      , A.oprargs=
                          [ A.CONST "1" "int4"
                          , A.CONST "42" "int4"
                          ]
                      }
                , A.targetresname = "lessThan"
                , A.resjunk = False
                }
              ]
          , A.resconstantqual = Nothing
          }

sort1 :: A.Operator
sort1 = A.SORT
        { A.targetlist =
            [ A.TargetEntry
                { A.targetexpr = A.VAR {A.varTable="grp", A.varColumn="a"}
                , A.targetresname = "foo"
                , A.resjunk = False
                }
            , A.TargetEntry 
                { A.targetexpr = A.VAR {A.varTable="grp", A.varColumn="b"}
                , A.targetresname = "bar"
                , A.resjunk = False
                }
            ]
        , A.operator =
            A.SEQSCAN
            { A.targetlist =
                [ A.TargetEntry
                    { A.targetexpr = A.VAR {A.varTable="grp", A.varColumn="a"}
                    , A.targetresname = "foo"
                    , A.resjunk = False
                    }
                , A.TargetEntry 
                    { A.targetexpr = A.VAR {A.varTable="grp", A.varColumn="b"}
                    , A.targetresname = "bar"
                    , A.resjunk = False
                    }
                ]
            , A.qual = []
            , A.scanrelation="grp"
            }
        , A.sortCols = [ A.SortEx 1 False True ]
        }

app1 :: A.Operator
app1 = A.APPEND
        { A.targetlist =
          [ A.TargetEntry
              { A.targetexpr = A.VAR {A.varTable="OUTER_VAR", A.varColumn="lessThan"}
              , A.targetresname = "bar"
              , A.resjunk = False
              }
          ]
        , A.appendplans = [func2, func2]
        }

agg1 :: A.Operator
agg1 = A.AGG
        { A.targetlist =
            [ A.TargetEntry
                { A.targetexpr =
                    A.AGGREF
                      { A.aggname = "sum"
                      , A.aggargs = [ A.TargetEntry
                                        { A.targetexpr = A.CONST "1" "int4"
                                        , A.targetresname = "foo"
                                        , A.resjunk = False
                                        }
                                    ]
                      , A.aggdirectargs = []
                      , A.aggorder = []
                      , A.aggdistinct = []
                      , A.aggfilter = Nothing
                      , A.aggstar = False
                      }
                , A.targetresname = "foo"
                , A.resjunk = False
                }
            ]
        , A.operator =
            A.RESULT
              { A.targetlist = []
              , A.resconstantqual = Nothing
              }
        , A.groupCols = []
        }

agg2 :: A.Operator
agg2 = A.AGG
        { A.targetlist =
            [ A.TargetEntry
                { A.targetexpr =
                    A.AGGREF
                      { A.aggname = "sum"
                      , A.aggargs = [ A.TargetEntry
                                        { A.targetexpr = A.VAR "OUTER_VAR" "a"
                                        , A.targetresname = "foo"
                                        , A.resjunk = False
                                        }
                                    ]
                      , A.aggdirectargs = []
                      , A.aggorder = []
                      , A.aggdistinct = []
                      , A.aggfilter = Nothing
                      , A.aggstar = False
                      }
                , A.targetresname = "foo"
                , A.resjunk = False
                }
            , A.TargetEntry
                { A.targetexpr = A.VAR "OUTER_VAR" "b"
                , A.targetresname = "b"
                , A.resjunk = False
                }
            ]
        , A.operator =
            A.SEQSCAN
              { A.targetlist =
                  [ A.TargetEntry
                      { A.targetexpr = A.VAR "grp" "a"
                      , A.targetresname = "a"
                      , A.resjunk = False
                      }
                  , A.TargetEntry
                      { A.targetexpr = A.VAR "grp" "b"
                      , A.targetresname = "b"
                      , A.resjunk = False
                      }
                  ]
              , A.qual = []
              , A.scanrelation = "grp"
              }
        , A.groupCols = []
        }

nestLoop1 :: A.Operator
nestLoop1 = A.NESTLOOP
            { A.targetlist =
                [ A.TargetEntry
                    { A.targetexpr = A.VAR "OUTER_VAR" "a"
                    , A.targetresname = "a"
                    , A.resjunk = False
                    }
                , A.TargetEntry
                    { A.targetexpr = A.VAR "OUTER_VAR" "b"
                    , A.targetresname = "b"
                    , A.resjunk = False
                    }
                , A.TargetEntry
                    { A.targetexpr = A.VAR "INNER_VAR" "x"
                    , A.targetresname = "x"
                    , A.resjunk = False
                    }
                , A.TargetEntry
                    { A.targetexpr = A.VAR "INNER_VAR" "y"
                    , A.targetresname = "y"
                    , A.resjunk = False
                    }
                ]
            , A.joinType = A.INNER
            , A.inner_unique = False
            , A.joinquals = []
            , A.nestParams = []
            , A.lefttree =
                A.SEQSCAN
                  { A.targetlist =
                      [ A.TargetEntry
                          { A.targetexpr = A.VAR "grp" "a"
                          , A.targetresname = "a"
                          , A.resjunk = False
                          }
                      , A.TargetEntry
                          { A.targetexpr = A.VAR "grp" "b"
                          , A.targetresname = "b"
                          , A.resjunk = False
                          }
                      ]
                  , A.qual = []
                  , A.scanrelation = "grp"
                  }
            , A.righttree =
                A.SEQSCAN
                  { A.targetlist =
                      [ A.TargetEntry
                          { A.targetexpr = A.VAR "grp" "a"
                          , A.targetresname = "x"
                          , A.resjunk = False
                          }
                      , A.TargetEntry
                          { A.targetexpr = A.VAR "grp" "b"
                          , A.targetresname = "y"
                          , A.resjunk = False
                          }
                      ]
                  , A.qual = []
                  , A.scanrelation = "grp"
                  }
            }

unique1 :: A.Operator
unique1 = A.UNIQUE
          { A.operator =
              A.SEQSCAN
                { A.targetlist = 
                    [ A.TargetEntry
                      { A.targetexpr = A.VAR "grp" "a"
                      , A.targetresname = "a"
                      , A.resjunk = False }
                    ]
                , A.qual = []
                , A.scanrelation = "grp"
                }
          , A.uniqueCols = [1]
          }

values1 :: A.Operator
values1 = A.VALUESSCAN
          { A.targetlist =
              [ A.TargetEntry
                { A.targetexpr = A.VALUESVAR 1
                , A.targetresname = "a"
                , A.resjunk = False }
              , A.TargetEntry
                { A.targetexpr = A.VALUESVAR 2
                , A.targetresname = "b"
                , A.resjunk = False }
              ]
          , A.qual = []
          , A.values_list =
              [ [ A.CONST "1" "int4", A.CONST "2" "int4"]
              , [ A.CONST "3" "int4", A.CONST "4" "int4"]
              ]
        }

projectset1 :: A.Operator
projectset1 = A.PROJECTSET
              { A.targetlist =
                  [ A.TargetEntry
                    { A.targetexpr =
                        A.FUNCEXPR
                        { A.funcname = "generate_series"
                        , A.funcargs =
                            [ A.CONST "1" "int4"
                            , A.CONST "10" "int4"
                            ]
                        }
                    , A.targetresname = "value"
                    , A.resjunk = False
                    }
                  ]
              , A.operator =
                  A.RESULT
                  { A.targetlist = []
                  , A.resconstantqual = Nothing
                  }
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
  -- putStrLn $ intercalate "\n" errs

  unless (null errs) $
    do
      error $ "AST is invalid:\n" ++ intercalate "\n" errs

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
  let infered = generatePlan tableDataR consts' (lgTableNames consts) (lgValuesScan consts) op
  
  -- Print AST structure as well as the postgres plan
  putStrLn $ PP.ppShow infered
  let pgplan = gprint infered
  putStrLn $ pgplan
  putStrLn $ "select _pq_plan_deserialize('" ++ pgplan ++ "');"


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

    checkAndGenerate authStr projectset1
