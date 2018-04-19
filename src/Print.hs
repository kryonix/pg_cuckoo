{-# LANGUAGE FlexibleInstances #-}

module Print ( Print.print ) where

import Data.Maybe
import PgPlan as O
import Text.PrettyPrint.ANSI.Leijen as L

print :: (Pretty a) => a -> String
print q = displayS (renderSmart 0.7 80 (pretty q)) ""

maybePretty :: (Pretty a) => Maybe a -> Doc
maybePretty Nothing = text "<>"
maybePretty (Just x) = pretty x

listPretty :: (Pretty a) => List a -> Doc
listPretty (List []) = text "<>"
listPretty e = pretty e

instance (Pretty a) => Pretty (List a) where
  pretty (List []) = parens empty
  pretty (List xs) = parens (hsep (map pretty xs))

instance Pretty PgBool where
  pretty (PgBool True) = text "true"
  pretty (PgBool False) = text "false"

instance Pretty Null where
  pretty Null = text "<>"

instance Pretty PlannedStmt where
  pretty x@(PlannedStmt {})
    = text "{" <> text "PLANNEDSTMT"
               <+> text ":commandType" <+> pretty (commandType x)
               <+> text ":queryId" <+> pretty (queryId x)
               <+> text ":hasReturning" <+> pretty (hasReturning x)
               <+> text ":hasModifyingCTE" <+> pretty (hasModifyingCTE x)
               <+> text ":canSetTag" <+> pretty (canSetTag x)
               <+> text ":transientPlan" <+> pretty (transientPlan x)
               <+> text ":dependsOnRole" <+> pretty (dependsOnRole x)
               <+> text ":parallelModeNeeded" <+> pretty (parallelModeNeeded x)
               <+> text ":planTree" <+> pretty (planTree x)
               <+> text ":rtable" <+> listPretty (rtable x)
               <+> text ":resultRelations" <+> pretty (resultRelations x)
               <+> text ":nonleafResultRelations" <+> pretty (nonleafResultRelations x)
               <+> text ":rootResultRelations" <+> pretty (rootResultRelations x)
               <+> text ":subplans" <+> listPretty (subplans x)
               <+> text ":rewindPlanIDs" <+> pretty (rewindPlanIDs x)
               <+> text ":rowMarks" <+> pretty (rowMarks x)
               <+> text ":relationOids" <+> pretty (relationOids x)
               <+> text ":invalItems" <+> pretty (invalItems x)
               <+> text ":nParamExec" <+> pretty (nParamExec x)
               <+> text ":utilityStmt" <+> pretty (utilityStmt x)
               <+> text ":stmt_location" <+> pretty (stmt_location x)
               <+> text ":stmt_len" <+> pretty (stmt_len x)
      <> text "}"

instance Pretty RelationList where
  pretty x@(RelationList lst) = parens (text "o" <> hsep (map pretty lst))

instance Pretty Bitmapset where
  pretty x@(Bitmapset lst) = parens (text "b" <> hsep (map pretty lst))

instance Pretty Seq where
  pretty x@(Seq {}) = pretty (seqlength x) <+> brackets (space <> (hsep (map pretty (seqvalues x))) <> space)

instance Pretty GenericPlan where
  pretty x@(GenericPlan {}) = text ":startup_cost" <+> pretty (startup_cost x)
                            <+> text ":total_cost" <+> pretty (total_cost x)
                            <+> text ":plan_rows" <+> pretty (plan_rows x)
                            <+> text ":plan_width" <+> pretty (plan_width x)
                            <+> text ":parallel_aware" <+> pretty (parallel_aware x)
                            <+> text ":parallel_safe" <+> pretty (parallel_safe x)
                            <+> text ":plan_node_id" <+> pretty (plan_node_id x)
                            <+> text ":targetlist" <+> pretty (targetlist x)
                            <+> text ":qual" <+> maybePretty (qual x)
                            <+> text ":lefttree" <+> maybePretty (lefttree x)
                            <+> text ":righttree" <+> maybePretty (righttree x)
                            <+> text ":initPlan" <+> maybePretty (initPlan x)
                            <+> text ":extParam" <+> pretty (extParam x)
                            <+> text ":allParam" <+> pretty (allParam x)

instance Pretty Plan where
  pretty x@(RESULT {}) = text "{RESULT"
                        <+> pretty (genericPlan x)
                        <+> text ":resconstantqual" <+> maybePretty (resconstantqual x)
                        <> text "}"
  
  pretty x@(PROJECTSET {}) = text "{PROJECTSET" <+> pretty (genericPlan x) <> text "}"
  pretty x@(SEQSCAN {}) = text "{SEQSCAN" <+> pretty (genericPlan x)
                          <+> text ":scanrelid" <+> pretty (scanrelid x)
                          <> text "}"
  pretty x@(LIMIT {}) = text "{LIMIT" <+> pretty (genericPlan x)
                        <+> text ":limitOffset" <+> offPP
                        <+> text ":limitCount" <+> cntPP
    where
      offPP = case (limitOffset x) of
                Just p -> pretty p
                Nothing -> text "<>"
      cntPP = case (limitCount x) of
                Just p -> pretty p
                Nothing -> text "<>"

instance Pretty RangeEx where
  pretty x@(RTE {}) = text "{RTE"
                      <+> text ":alias" <+> aliasPP
                      <+> text ":eref" <+> pretty (eref x)
                      <+> text ":rtekind" <+> pretty (rtekind x)
                      <+> text ":relid" <+> pretty (relid x)
                      <+> text ":relkind" <+> pretty (relkind x)
                      <+> text ":tablesample" <+> pretty (tablesample x)
                      <+> text ":lateral" <+> pretty (lateral x)
                      <+> text ":inh" <+> pretty (inh x)
                      <+> text ":inFromCl" <+> pretty (inFromCl x)
                      <+> text ":requiredPerms" <+> pretty (requiredPerms x)
                      <+> text ":checkAsUser" <+> pretty (checkAsUser x)
                      <+> text ":selectedCols" <+> pretty (selectedCols x)
                      <+> text ":insertedCols" <+> pretty (insertedCols x)
                      <+> text ":updatedCols" <+> pretty (updatedCols x)
                      <+> text ":securityQuals" <+> pretty (securityQuals x)
                      <> text "}"
    where
      aliasPP = case alias x of
                  Just x  -> pretty x
                  Nothing -> text "<>"

instance Pretty Alias where
  pretty x@(Alias {}) = text "{ALIAS"
                        <+> text ":aliasname" <+> pretty (aliasname x)
                        <+> text ":colnames" <+> parens (hsep (map (dquotes . pretty) ((\(List p) -> p) $ colnames x)))
                        <> text "}"

instance Pretty TargetEntry where
  pretty x@(TargetEntry {}) = text "{TARGETENTRY"
                            <+> text ":expr" <+> pretty (expr x)
                            <+> text ":resno" <+> pretty (resno x)
                            <+> text ":resname" <+> maybePretty (resname x)
                            <+> text ":ressortgroupref" <+> pretty (ressortgroupref x)
                            <+> text ":resorigtbl" <+> pretty (resorigtbl x)
                            <+> text ":resorigcol" <+> pretty (resorigcol x)
                            <+> text "resjunk" <+> pretty (resjunk x)
                            <> text "}"

instance Pretty Expr where
  pretty x@(VAR {}) = text "{VAR"
                      <+> text ":varno" <+> pretty (varno x)
                      <+> text ":varattno" <+> pretty (varattno x)
                      <+> text ":vartype" <+> pretty (vartype x)
                      <+> text ":vartypmod" <+> pretty (vartypmod x)
                      <+> text ":varcollid" <+> pretty (varcollid x)
                      <+> text ":varlevelsup" <+> pretty (varlevelsup x)
                      <+> text ":varnoold" <+> pretty (varnoold x)
                      <+> text ":varoattno" <+> pretty (varoattno x)
                      <+> text ":location" <+> pretty (location x)
                      <> text "}"

  pretty x@(CONST {}) = text "{CONST"
                        <+> text ":consttype" <+> pretty (consttype x)
                        <+> text ":consttypmod" <+> pretty (consttypmod x)
                        <+> text ":constcollid" <+> pretty (constcollid x)
                        <+> text ":constlen" <+> pretty (constlen x)
                        <+> text ":constbyval" <+> pretty (constbyval x)
                        <+> text ":constisnull" <+> pretty (constisnull x)
                        <+> text ":location" <+> pretty (location x)
                        <+> text ":constvalue" <+> constVal
                        <> text "}"
    where
      constVal = case constvalue x of
                  Just p  -> pretty p
                  Nothing -> text "<>"

