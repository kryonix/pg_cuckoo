{-|
Module      : GenGraphViz
Description : Generates a graphviz representation of the relational algebra AST
Author      : Denis Hirn
-}

{-# LANGUAGE FlexibleInstances, NamedFieldPuns #-}

module GenGraphViz where

import OperSemIO
import qualified Data.List as DL
import qualified Data.String.Utils as DS
import qualified Data.Maybe as MB

import           Text.PrettyPrint.ANSI.Leijen as L 
import Text.Show.Pretty as PP hiding (List, Value, Float)

import LogicalAST as A
import LogicalASTPP as AP

data Context = C

data StateI = StateI { count :: Integer }

fresh :: Rule String String
fresh v = do
  n <- lift $ gets count
  lift $ modify (\x -> StateI (count x+1))
  return (v ++ show n)

type Log = [Edge]

newtype Edge = E (String, String)
data Graph = Graph String [Node] [Edge]
data Node = Node Doc Doc

prettyG :: Graph -> String
prettyG q = displayS (renderSmart 0.7 80 (pretty q)) ""

esc :: Pretty a => a -> Doc
esc d = text $ DS.replace "\"" "\\\"" $ displayS (renderSmart 0.7 80 (pretty d)) ""

instance Pretty Graph where
  pretty (Graph n v e) = text "graph" <+> text n 
                        L.<$> braces
                              (vcat $ 
                                [ text "node [shape=rectangle];"
                                , text "graph [fontname = \"fira code\"];"
                                , text "node [fontname = \"fira code\"];"
                                , text "edge [fontname = \"fira code\"];"
                                ] ++ (map pretty v) ++ (map pretty e))

instance Pretty Node where
  pretty (Node s l) = s <+> brackets (text "label=" <> dquotes l) <> semi

instance Pretty Edge where
  pretty (E (a, b)) = text a <+> text "--" <+> text b <> semi

br = string "\n" -- angles (text "BR/")

type Rule a b = a -> OperSemIO StateI Context b Log

gen :: A.Plan -> IO String
gen e = do
              ((erg, _), state) <- runOperSemSTIO (start e) (StateI 0) C
              let res = case erg of
                        Prelude.Left str -> error $ "Inference error: " ++ str
                        Prelude.Right a -> a
              return $ prettyG res

start :: Rule A.Plan Graph
start p = do
  (_, n, e) <- trPlan p
  return $ Graph "g" n e

trPlan :: Rule A.Plan (String, [Node], [Edge])
trPlan (Projection {targetList, leftTree})
  = do
    name <- fresh "proj"
    (child, n, e) <- trPlan leftTree
    return ( name
           , n++[ Node (text name) $ 
                  text "Π" <> br <> cat (punctuate comma (map esc targetList)) ]
           , e++[E (name, child)])

trPlan (Scan {relation, alias, colnames})
  = do
    name <- fresh "scan"
    return ( name
           , [ Node (text name) $
              text "Scan"
              <> colon
              <+> text relation
              <+> text alias
              -- <> br
              -- <> cat (punctuate comma (map esc colnames))
              ], [])

trPlan (Selection {predicate, leftTree})
  = do
    name <- fresh "sel"
    (child, n, e) <- trPlan leftTree
    return ( name
           , n++[Node (text name) $ text "σ" <> br <> cat (punctuate comma (map esc predicate)) ]
           , e++[E (name, child)])

trPlan (Join {leftTree, rightTree, predicate, joinType})
  = do
    name <- fresh "join"
    (childL, nL, eL) <- trPlan leftTree
    (childR, nR, eR) <- trPlan rightTree

    let symbol = case joinType of
                  INNER -> "⨝"
                  LEFT  -> "⟕"
                  FULL  -> "⟗"
                  RIGHT -> "⟖"
                  SEMI  -> "⋉"
                  ANTI  -> "▷"

    return ( name
           , nL ++ nR ++
             [ Node (text name) $ text symbol <> br <> cat (punctuate comma (map esc predicate)) ]
           , eL ++ eR ++
             [E (name, childL), E (name, childR) ])

trPlan (DependentJoin {leftTree, rightTree, binding, predicate, joinType})
  = do
    name <- fresh "depjoin"
    (childL, nL, eL) <- trPlan leftTree
    (childR, nR, eR) <- trPlan rightTree

    return ( name
          , nL ++ nR ++
            [ Node (text name) $ text "⧑" <> parens (text (show joinType)) <> br <> cat (punctuate comma (map esc predicate))
              <> br <> cat (punctuate comma (map esc binding)) ]
          , eL ++ eR ++
            [E (name, childL), E (name, childR) ])

trPlan (GroupBy {leftTree, targetList, groupCols})
  = do
    name <- fresh "groupby"
    (child, n, e) <- trPlan leftTree
    return ( name
           , n++[Node (text name) $ text "Γ" <> br <> cat (punctuate comma (map esc targetList)) ]
           , e++[E (name, child)])

trPlan (Limit {leftTree, limitCount, limitOffset})
  = do
    name <- fresh "limit"
    (child, n, e) <- trPlan leftTree
    return ( name
           , n ++ [Node (text name) (text "Limit")]
           , e ++ [E (name, child)])

trPlan (Sort {leftTree, sortCols})
  = do
    name <- fresh "sort"
    (child, n, e) <- trPlan leftTree
    return ( name
           , n ++ [Node (text name) $ text "Sort"]
           , e ++ [E (name, child)])

trPlan (D {leftTree})
  = do
    name <- fresh "d"
    (child, n, e) <- trPlan leftTree
    return ( name
           , n++[Node (text name) $ text "D" ]
           , e++[E (name, child)])

trPlan (Constant { targetList })
  = do
    name <- fresh "constant"
    return (name, [Node (text name) $ text "Constant" <> br <> cat (punctuate comma (map esc targetList))]
           , []
           )

trPlan (Rename { leftTree })
  = do
    name <- fresh "rename"

    (child, n, e) <- trPlan leftTree

    return (name, n++[Node (text name) $ text "Rename"], e++[E (name, child)])


trPlan e = error $ "Not implemented: " ++ take 10 (show e)