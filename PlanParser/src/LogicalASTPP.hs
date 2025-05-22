
{-|
Module      : LogicalASTPP
Description : 
Copyright   : 
License     : AllRightsReserved
Maintainer  : Denis Hirn
-}

{-# LANGUAGE FlexibleInstances, NamedFieldPuns #-}

module LogicalASTPP (prettyP, pretty) where

import           Prelude hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen as L 
import Text.Show.Pretty hiding (List, Value, Float)
import Data.Maybe
import LogicalAST as A
import qualified Data.List as DL


prettyP :: Plan -> String
prettyP q = displayS (renderSmart 0.7 80 (pretty q)) ""


sqlList :: [Doc] -> Doc
sqlList x = cat (punctuate (comma <> space) x)

sqlList' :: Doc -> [Doc] -> Doc
sqlList' s x = align (cat (punctuate (s <> space) x))

instance Pretty Plan where
  pretty (D (Projection {targetList, leftTree}))
    = text "SELECT DISTINCT" <+> align (sqlList (map pretty targetList))
      <$> text "FROM"
      <+> align ((parens (pretty' leftTree)) <+> text "AS l")

  pretty (Projection {targetList, leftTree})
    = text "SELECT" <+> align (sqlList (map pretty targetList))
      <$> text "FROM"
      <+> align ((parens (pretty' leftTree)) <+> text "AS l")

  pretty (Join {leftTree, rightTree, predicate, joinType = INNER })
    = text "SELECT " <> sqlList (map text vars')
      <$> text "FROM"
      <+> align (vcat ([ parens (pretty' leftTree) <+> text "AS l" <> text ","
               , parens (pretty' rightTree) <+> text "AS r"
               ]))
      <> (if null predicate then empty else (empty <$> text "WHERE" <+> sqlList' (text " AND") (map pretty predicate)))
    where
      vL = prodVars leftTree
      vR = prodVars rightTree
      vars = DL.nub $ vL ++ vR
      vars' = [ if x `elem` vL then "l.\"" ++ x ++ "\"" else "r.\"" ++ x ++ "\"" | x <- vars]

  pretty (Join {leftTree, rightTree, predicate = ([Operator "=" [l, r]]) , joinType = SEMI })
    = text "SELECT " <> sqlList (map text vars')
      <$> text "FROM"
      <+> align (vcat ([ parens (pretty' leftTree) <+> text "AS l"
               -- , parens (pretty' rightTree) <+> text "AS r"
               ]))
      <> (empty <$> text "WHERE" 
                <+> pretty l 
                <+> text "IN" 
                <+> parens (text "SELECT" <+> pretty r
                                          <+> text "FROM" 
                                          <+> parens (pretty' rightTree)
                                          <+> text "AS r"))
    where
      vL = prodVars leftTree
      vR = prodVars rightTree
      vars = DL.nub $ vL
      vars' = [ "l.\"" ++ x ++ "\""| x <- vars ]

  pretty (Join {leftTree, rightTree, predicate, joinType = FULL })
    = text "SELECT " <> sqlList (map text vars')
      <$> text "FROM"
      <+> align (vcat ([ parens (pretty' leftTree) <+> text "AS l" <> text " FULL OUTER JOIN "
               , parens (pretty' rightTree) <+> text "AS r"
               ]))
      <+> if null predicate then text "ON (true)"
          else ((empty <$> text "ON" <+> parens (sqlList' (text " AND") (map pretty predicate))))
    where
      vL = prodVars leftTree
      vR = prodVars rightTree
      vars = DL.nub $ vL ++ vR
      vars' = [ if x `elem` vL then "l.\"" ++ x ++ "\"" else "r.\"" ++ x ++ "\"" | x <- vars]

  pretty (Join {leftTree, rightTree, predicate, joinType = LEFT })
    = text "SELECT " <> sqlList (map text vars')
      <$> text "FROM"
      <+> align (vcat ([ parens (pretty' leftTree) <+> text "AS l" <> text " LEFT OUTER JOIN "
               , parens (pretty' rightTree) <+> text "AS r"
               ]))
      <+> if null predicate then text "ON (true)"
          else ((empty <$> text "ON" <+> parens (sqlList' (text " AND") (map pretty predicate))))
    where
      vL = prodVars leftTree
      vR = prodVars rightTree
      vars = DL.nub $ vL ++ vR
      vars' = [ if x `elem` vL then "l.\"" ++ x ++ "\"" else "r.\"" ++ x ++ "\"" | x <- vars]


  pretty (Selection {predicate, leftTree})
     = text "SELECT *"
      <$> text "FROM" <+> align (parens (pretty' leftTree) <+> text "AS l")
      <> (if null predicate 
          then empty 
          else (empty <$> text "WHERE" <+> sqlList' (text " AND") (map pretty predicate)))
  
  pretty (DependentJoin {leftTree, rightTree, predicate})
    = text "SELECT *"
      <$> text "FROM"
      <+> align (vcat ([ parens (pretty' leftTree) <+> text "AS l" <> text ", lateral"
                          , parens (pretty' rightTree) <+> text "AS r"]))
      <> (if null predicate then empty else (empty <$> text "WHERE" <+> sqlList' (text " AND") (map pretty predicate)))
  
  pretty (GroupBy {leftTree, groupCols, targetList})
    = text "SELECT" <+> align (sqlList ((map pretty targetList))) -- ++ (map pretty groupCols))
      <$> text "FROM"
      <+> align (parens (pretty' leftTree) <+> text "AS l") <>
      (if null groupCols then empty else (empty <$> text "GROUP BY" <+> sqlList (map pretty groupCols)))

  pretty (Map {leftTree, targetList})
    = text "SELECT *," <+> sqlList (map pretty targetList)
      <$> text "FROM"
      <+> align (parens (pretty' leftTree) <+> text "AS l")
  
  pretty (Scan {relation, alias, colnames})
    = vcat
    [ text "SELECT *"
    , text "FROM" <+> text relation
      <+> text "AS" <+> text alias -- <> parens (align (sqlList (map pretty colnames)))
    ]

  pretty (Constant { targetList })
    = text "SELECT" <+> sqlList (map pretty targetList)

  pretty (Rename { leftTree }) = pretty leftTree
  pretty (Limit { leftTree, limitCount, limitOffset })
    = vcat $
      [ text "SELECT *"
      , text "FROM" <+> align (parens (pretty leftTree) <+> text "AS l")
      ] ++ (if isJust limitCount then [text "LIMIT" <+> pretty (fromJust limitCount)] else [])
      ++ (if isJust limitOffset then [text "OFFSET" <+> pretty (fromJust limitOffset)] else [])

  pretty (Sort { sortCols, leftTree })
    = vcat $
      [ text "SELECT *"
      , text "FROM" <+> align (parens (pretty leftTree) <+> text "AS l")
      , text "ORDER BY" <+> align (sqlList (map pretty sortCols))
      ]

  pretty e = error $ take 20 (show e)

pretty' :: Plan -> Doc
pretty' (Scan {relation}) = text "table" <+> text relation
pretty' a = pretty a

--  pretty (D {leftTree}) = pretty leftTree

instance Pretty SortEx where
  pretty (SortEx { sortTarget, sortASC, sortNullsFirst })
    = pretty sortTarget <> (if not sortASC then text " DESC" else empty)

instance Pretty TargetExpr where
  pretty (TargetExpr {expr, resname = Nothing}) = pretty expr
  pretty (TargetExpr {expr, resname = Just x}) = pretty expr <+> text "AS" <+> dquotes (text x)

instance Pretty Expr where
  pretty (Const {value}) = squotes (text value)
  pretty (Function {function="numeric", args=[a]}) = pretty a
  pretty (Operator {operator, args=[a1, a2]}) = parens $ pretty a1 <+> text operator <+> pretty a2
  pretty (Operator {operator, args=[a1]}) = text operator <> parens (pretty a1)
  pretty (Function {function, args}) = text function <> parens (sqlList (map pretty args))
  pretty (VAR {tableName, colName}) = text tableName <> text "." <> dquotes (text colName)
  pretty (VARSUB {colName}) = text colName
  pretty (AGG {aggname, args, star})
    = text aggname <> parens (if not star then sqlList (map pretty args) else text "*")
  pretty (BoolOp {operator, args}) = parens (sqlList' (space <> text operator) (map pretty args))
  pretty (NullTest {arg, isnull}) = pretty arg <+> text (if isnull then "IS NULL" else "IS NOT NULL")
  pretty (DEP { arg }) = pretty arg
  pretty (Param pID) = text "PARAM(" <> pretty pID <> text ")"
  pretty e = error $ "pretty not implemented for: " ++ show e

prodVars :: Plan -> [String]
prodVars Projection { targetList, leftTree } = map targetExToVar targetList
prodVars Selection { predicate, leftTree } = prodVars leftTree
prodVars Rename { rename, leftTree } = prodVars leftTree
prodVars Union { leftTree, rightTree } = prodVars leftTree
prodVars Difference { leftTree, rightTree } = prodVars leftTree
prodVars Join { leftTree, rightTree, predicate } = prodVars leftTree ++ prodVars rightTree
prodVars Sort { leftTree } = prodVars leftTree
prodVars DependentJoin { leftTree, rightTree, predicate } = prodVars leftTree ++ prodVars rightTree
prodVars GroupBy { leftTree, targetList } = map targetExToVar targetList
prodVars Map { leftTree, targetList } = map targetExToVar targetList
prodVars Limit { leftTree } = prodVars leftTree

-- prodVars Scan { colnames } = colnames
prodVars Scan { alias, colnames } = colnames
-- prodVars Scan { alias, colnames } = colnames ++ map (\x -> "l." ++ x) colnames ++ map (\x -> alias ++ "." ++ x) colnames
prodVars Constant { targetList } = map targetExToVar targetList
prodVars D { leftTree } = prodVars leftTree

targetExToVar :: TargetExpr -> String
targetExToVar (TargetExpr { resname = Just s }) = s


-- SELECT *
-- FROM
--     (SELECT *
--     FROM
--         (SELECT *
--         FROM
--             (SELECT * FROM exams AS e) AS r,
--             (SELECT * FROM students AS s) AS l
--         WHERE l.id = r.sid) AS l, lateral
--         (SELECT min(l.grade) as m
--         FROM (SELECT *
--         FROM
--             (SELECT * FROM exams AS e2) AS l
--         WHERE id = l.sid) AS l) AS r
--     ) AS l
-- WHERE l.grade = m