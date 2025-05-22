{-|
Module      : ParsePlan
Description : This module checks whether a given input AST is valid or not
Author      : Denis Hirn

This module does a traversal of the given AST.
Errors are collected and can then be used for error messages.
-}

{-# LANGUAGE NamedFieldPuns #-}

module ParsePlan ( parse ) where

import OperSem
import Reader as R
import qualified Database.PgCuckoo.PgPlan as O

import Data.Maybe

import qualified Text.Show.Pretty as PP

import qualified Data.Map as M

data StateI = StateI { count :: Integer }
data Log = Log { lgConst :: [ (O.Expr, String) ] }

instance Monoid Log where
  mempty = Log []
  (Log e1) `mappend` (Log e2) = Log (e1++e2)

logConst :: Rule (O.Expr, String) ()
logConst c = tell (Log [c])

fresh :: Rule String String
fresh v = do
  n <- lift $ gets count
  lift $ modify (\x -> StateI (count x+1))
  return (v ++ show n)

type Rule a b = a -> OperSem StateI () b Log

-- | Three parameter rule
type Rule3 a b c d = a -> b -> c -> OperSem StateI () d Log

-- | Validates an operator and returns all errors
parse :: R.Chunk -> (O.PLANNEDSTMT, [(O.Expr, String)])
parse op = let
              (erg, lg) = runOperSem ((+>) op) (StateI 0) ()
              (Log lg') = lg
              res = case erg of
                Prelude.Left str -> error $ "Inference error: " ++ str
                Prelude.Right a -> a
              in (res, lg')

(!) :: (Show a, Ord a) => M.Map a b -> a -> b
fs ! key = fromMaybe
            (error $ "given key " ++ PP.ppShow key ++ " is not an element in the map")
            (M.lookup key fs)

(+>) :: Rule R.Chunk O.PLANNEDSTMT
(+>) (R.Chunk "PLANNEDSTMT" fs)
  = do
    let (Int commandType) = fs ! "commandType"
    let (Int queryId)     = fs ! "queryId"
    let (Bool hasReturning) = fs ! "hasReturning"
    let (Bool hasModifyingCTE) = fs ! "hasModifyingCTE"
    let (Bool canSetTag)       = fs ! "canSetTag"
    let (Bool transientPlan)   = fs ! "transientPlan"
    let (Bool dependsOnRole)   = fs ! "dependsOnRole"
    let (Bool parallelModeNeeded) = fs ! "parallelModeNeeded"

    planTree <- extractVChunk "planTree" trPlan fs
    subplans <- extractListM "subplans" trPlan fs

    let rewindPlanIDs    = extractIntList "rewindPlanIDs" fs
    let relationOids     = extractIntList "relationOids" fs
    let (Int nParamExec) = fs ! "nParamExec"

    rtable <- extractListM "rtable" trRangeEx fs

    return $ O.PLANNEDSTMT
              { O.commandType            = commandType
              , O.queryId                = queryId
              , O.hasReturning           = O.PgBool hasReturning
              , O.hasModifyingCTE        = O.PgBool hasModifyingCTE
              , O.canSetTag              = O.PgBool canSetTag
              , O.transientPlan          = O.PgBool transientPlan
              , O.dependsOnRole          = O.PgBool dependsOnRole
              , O.parallelModeNeeded     = O.PgBool parallelModeNeeded
              , O.planTree               = planTree
              , O.rtable                 = O.List rtable
              , O.resultRelations        = O.Null
              , O.nonleafResultRelations = O.Null
              , O.rootResultRelations    = O.Null
              , O.subplans               = O.List subplans
              , O.rewindPlanIDs          = O.Bitmapset rewindPlanIDs
              , O.rowMarks               = O.Null
              , O.relationOids           = O.RelationList relationOids
              , O.invalItems             = O.Null
              , O.nParamExec             = nParamExec
              , O.utilityStmt            = O.Null
              , O.stmt_location          = -1
              , O.stmt_len               = -1
              }

trGenericPlan :: Rule R.Chunk O.GenericPlan
trGenericPlan (R.Chunk _ fs)
  = do
    let (Bool parallel_aware) = fs ! "parallel_aware"
    let (Bool parallel_safe)  = fs ! "parallel_safe"
    let (Int plan_node_id)    = fs ! "plan_node_id"

    lefttree <- extractMaybeVChunkM "lefttree" trPlan fs
    righttree <- extractMaybeVChunkM "righttree" trPlan fs

    initPlan <- extractListM "initPlan" trExpr fs
    qual     <- extractListM "qual" trExpr fs
    
    targetlist <- extractListM "targetlist" trTargetEx fs
    
    let extParam = extractIntList "extParam" fs
    let allParam = extractIntList "allParam" fs

    return $ O.GenericPlan
              { O.startup_cost   = 0.0
              , O.total_cost     = 0.0
              , O.plan_rows      = 0
              , O.plan_width     = 0
              , O.parallel_aware = O.PgBool parallel_aware
              , O.parallel_safe  = O.PgBool parallel_safe
              , O.plan_node_id   = plan_node_id
              , O.targetlist     = O.List targetlist
              , O.qual           = O.List qual
              , O.lefttree       = lefttree
              , O.righttree      = righttree
              , O.initPlan       = O.List initPlan
              , O.extParam       = O.Bitmapset extParam
              , O.allParam       = O.Bitmapset allParam
              }

trPlan :: Rule R.Chunk O.Plan
trPlan c@(R.Chunk "RESULT" fs)
  = do
    genericPlan <- trGenericPlan c
    resconstantqual' <- extractListM "resconstantqual" trExpr fs

    let resconstantqual = case resconstantqual' of
                          [] -> Nothing
                          x:_ -> Just x
                          
    return $ O.RESULT
              { O.genericPlan = genericPlan
              , O.resconstantqual = resconstantqual
              }

trPlan c@(R.Chunk "PROJECTSET" fs)
  = do
    genericPlan <- trGenericPlan c
    return O.PROJECTSET
            { O.genericPlan = genericPlan }

trPlan c@(R.Chunk "SEQSCAN" fs)
  = do
    genericPlan <- trGenericPlan c
    let (Int scanrelid) = fs ! "scanrelid"

    return $ O.SEQSCAN
              { O.genericPlan = genericPlan
              , O.scanrelid   = scanrelid
              }

trPlan c@(R.Chunk "LIMIT" fs)
  = do
    genericPlan <- trGenericPlan c
    limitOffset <- extractMaybeVChunkM "limitOffset" trExpr fs
    limitCount  <- extractMaybeVChunkM "limitCount" trExpr fs

    return O.LIMIT
            { O.genericPlan = genericPlan
            , O.limitOffset = limitOffset
            , O.limitCount  = limitCount
            }

trPlan c@(R.Chunk "SORT" fs)
  = do
    genericPlan <- trGenericPlan c

    let (Int numCols) = fs ! "numCols"
    let sortColIdx    = extractIntList "sortColIdx" fs
    let sortOperators = extractIntList "sortOperators" fs
    let collations    = extractIntList "collations" fs
    let nullsFirst    = extractBoolList "nullsFirst" fs
    return O.SORT
            { O.genericPlan   = genericPlan
            , O.numCols       = numCols
            , O.sortColIdx    = O.PlainList sortColIdx
            , O.sortOperators = O.PlainList sortOperators
            , O.collations    = O.PlainList collations
            , O.nullsFirst    = O.PlainList $ map O.PgBool nullsFirst
            }

trPlan c@(R.Chunk "GROUP" fs)
  = do
    genericPlan <- trGenericPlan c

    let (Int numCols) = fs ! "numCols"
    let grpColIdx    = extractIntList "grpColIdx" fs
    let grpOperators = extractIntList "grpOperators" fs

    return O.GROUP
            { O.genericPlan = genericPlan
            , O.numCols       = numCols
            , O.grpColIdx    = O.PlainList grpColIdx
            , O.grpOperators = O.PlainList grpOperators
            }

trPlan c@(R.Chunk "APPEND" fs)
  = do
    genericPlan <- trGenericPlan c

    appendplans <- extractListM "appendplans" trPlan fs

    return O.APPEND
            { O.genericPlan      = genericPlan
            , O.partitioned_rels = O.Null
            , O.appendplans      = O.List appendplans
            }

trPlan c@(R.Chunk "MERGEAPPEND" fs)
  = do
    genericPlan <- trGenericPlan c

    mergeplans <- extractListM "mergeplans" trPlan fs
    let (Int numCols) = fs ! "numCols"
    let sortColIdx    = extractIntList "sortColIdx" fs
    let sortOperators = extractIntList "sortOperators" fs
    let collations    = extractIntList "collations" fs
    let nullsFirst    = extractBoolList "nullsFirst" fs

    return O.MERGEAPPEND
            { O.genericPlan      = genericPlan
            , O.partitioned_rels = O.Null
            , O.mergeplans       = O.List mergeplans
            , O.numCols          = numCols
            , O.sortColIdx       = O.PlainList sortColIdx
            , O.sortOperators    = O.PlainList sortOperators
            , O.collations       = O.PlainList collations
            , O.nullsFirst       = O.PlainList $ map O.PgBool nullsFirst
            }

trPlan c@(R.Chunk "RECURSIVEUNION" fs)
  = do
    genericPlan <- trGenericPlan c

    let (Int wtParam) = fs ! "wtParam"
    let (Int numCols) = fs ! "numCols"
    let dupColIdx     = extractIntList "dupColIdx" fs
    let dupOperators  = extractIntList "dupOperators" fs
    let (Int numGroups) = fs ! "numGroups"

    return O.RECURSIVEUNION
            { O.genericPlan  = genericPlan
            , O.wtParam      = wtParam
            , O.numCols      = numCols
            , O.dupColIdx    = O.PlainList dupColIdx
            , O.dupOperators = O.PlainList dupOperators
            , O.numGroups    = numGroups
            }

trPlan c@(R.Chunk "WORKTABLESCAN" fs)
  = do
    genericPlan <- trGenericPlan c
    let (Int scanrelid) = fs ! "scanrelid"
    let (Int wtParam)   = fs ! "wtParam"
    return O.WORKTABLESCAN
            { O.genericPlan = genericPlan
            , O.scanrelid   = scanrelid
            , O.wtParam     = wtParam
            }

trPlan c@(R.Chunk "BITMAPAND" fs)
  = do
    genericPlan <- trGenericPlan c
    bitmapplans <- extractListM "bitmapplans" trPlan fs

    return O.BITMAPAND
            { O.genericPlan = genericPlan
            , O.bitmapplans = O.List bitmapplans
            }

trPlan c@(R.Chunk "BITMAPOR" fs)
  = do
    genericPlan <- trGenericPlan c
    let (Bool isshared) = fs ! "isshared"
    bitmapplans <- extractListM "bitmapplans" trPlan fs

    return O.BITMAPOR
            { O.genericPlan = genericPlan
            , O.isshared    = O.PgBool isshared
            , O.bitmapplans = O.List bitmapplans
            }

trPlan c@(R.Chunk "INDEXSCAN" fs)
  = do
    genericPlan <- trGenericPlan c
    let (Int scanrelid) = fs ! "scanrelid"
    let (Int indexid)   = fs ! "indexid"
    indexqual <- extractListM "indexqual" trExpr fs

    let indexorderby = case fs ! "indexorderby" of
                      NoValue -> Nothing
                      _       -> Just $ O.PlainList $ extractIntList "indexorderby" fs

    let indexorderbyops = case fs ! "indexorderbyops" of
                      NoValue -> Nothing
                      _       -> Just $ O.PlainList $ extractIntList "indexorderbyops" fs

    let (Int indexorderdir) = fs ! "indexorderdir"

    return O.INDEXSCAN
            { O.genericPlan      = genericPlan
            , O.scanrelid        = scanrelid
            , O.indexid          = indexid
            , O.indexqual        = O.List indexqual
            , O.indexqualorig    = O.Null
            , O.indexorderby     = indexorderby
            , O.indexorderbyorig = O.Null
            , O.indexorderbyops  = indexorderbyops
            , O.indexorderdir    = indexorderdir }

trPlan c@(R.Chunk "INDEXONLYSCAN" fs)
  = do
    genericPlan <- trGenericPlan c
    let (Int scanrelid) = fs ! "scanrelid"
    let (Int indexid)   = fs ! "indexid"
    indexqual <- extractListM "indexqual" trExpr fs

    let indexorderby = case fs ! "indexorderby" of
                      NoValue -> Nothing
                      _       -> Just $ O.PlainList $ extractIntList "indexorderby" fs

    indextlist <- extractListM "indextlist" trTargetEx fs
    let (Int indexorderdir) = fs ! "indexorderdir"

    return O.INDEXONLYSCAN
            { O.genericPlan   = genericPlan
            , O.scanrelid     = scanrelid
            , O.indexid       = indexid
            , O.indexqual     = O.List indexqual
            , O.indexorderby  = indexorderby
            , O.indextlist    = O.List indextlist
            , O.indexorderdir = indexorderdir 
            }

trPlan c@(R.Chunk "BITMAPINDEXSCAN" fs)
  = do
    genericPlan <- trGenericPlan c

    let (Int scanrelid) = fs ! "scanrelid"
    let (Int indexid)   = fs ! "indexid"
    let (Bool isshared) = fs ! "isshared"
    indexqual <- extractListM "indexqual" trExpr fs

    return O.BITMAPINDEXSCAN
            { O.genericPlan = genericPlan
            , O.scanrelid   = scanrelid
            , O.indexid     = indexid
            , O.isshared    = O.PgBool isshared
            , O.indexqual   = O.List indexqual
            , O.indexqualorig = O.Null
            }

trPlan c@(R.Chunk "BITMAPHEAPSCAN" fs)
  = do
    genericPlan <- trGenericPlan c
    let (Int scanrelid) = fs ! "scanrelid"
    bitmapqualorig <- extractListM "bitmapqualorig" trExpr fs

    return O.BITMAPHEAPSCAN
            { O.genericPlan = genericPlan
            , O.scanrelid   = scanrelid
            , O.bitmapqualorig = O.List bitmapqualorig
            }

trPlan c@(R.Chunk "AGG" fs)
  = do
    genericPlan <- trGenericPlan c
    let (Int aggstrategy) = fs ! "aggstrategy"
    let (Int aggsplit)    = fs ! "aggsplit"
    let (Int numCols)     = fs ! "numCols"
    let grpColIdx         = extractIntList "grpColIdx" fs
    let grpOperators      = extractIntList "grpOperators" fs
    let (Int numGroups)   = fs ! "numGroups"
    let aggParams         = extractIntList "aggParams" fs
    
    return O.AGG
            { O.genericPlan  = genericPlan
            , O.aggstrategy  = aggstrategy
            , O.aggsplit     = aggsplit
            , O.numCols      = numCols
            , O.grpColIdx    = O.PlainList grpColIdx
            , O.grpOperators = O.PlainList grpOperators
            , O.numGroups    = numGroups
            , O.aggParams    = O.Bitmapset aggParams
            , O.groupingSets = O.Null
            , O.chain        = O.Null
            }

trPlan c@(R.Chunk "WINDOWAGG" fs)
  = do
    genericPlan <- trGenericPlan c

    let (Int winref)       = fs ! "winref"
    let (Int partNumCols)  = fs ! "partNumCols"
    let partColIdx         = extractIntList "partColIdx" fs
    let partOperators      = extractIntList "partOperators" fs
    let (Int ordNumCols)   = fs ! "ordNumCols"
    let ordColIdx          = extractIntList "ordColIdx" fs
    let ordOperators       = extractIntList "ordOperators" fs
    let (Int frameOptions) = fs ! "frameOptions"
    
    startOffset <- extractMaybeVChunkM "startOffset" trExpr fs
    endOffset   <- extractMaybeVChunkM "endOffset" trExpr fs
  
    return O.WINDOWAGG
            { O.genericPlan   = genericPlan
            , O.winref        = winref
            , O.partNumCols   = partNumCols
            , O.partColIdx    = O.PlainList partColIdx
            , O.partOperators = O.PlainList partOperators
            , O.ordNumCols    = ordNumCols
            , O.ordColIdx     = O.PlainList ordColIdx
            , O.ordOperators  = O.PlainList ordOperators
            , O.frameOptions  = frameOptions
            , O.startOffset   = startOffset
            , O.endOffset     = endOffset
            }

trPlan c@(R.Chunk "MATERIAL" fs)
  = do
    genericPlan <- trGenericPlan c
    return O.MATERIAL
            { O.genericPlan = genericPlan }

trPlan c@(R.Chunk "NESTLOOP" fs)
  = do
    genericPlan <- trGenericPlan c
    let (Int jointype) = fs ! "jointype"
    let (Bool inner_unique) = fs ! "inner_unique"
    joinquals <- extractListM "joinqual" trExpr fs
    nestparams <- extractListM "nestParams" trNestParam fs

    return $ O.NESTLOOP
              { O.genericPlan = genericPlan
              , O.jointype    = jointype
              , O.inner_unique = O.PgBool inner_unique
              , O.joinquals    = O.List joinquals
              , O.nestParams   = O.List nestparams
              }

trPlan c@(R.Chunk "MERGEJOIN" fs)
  = do
    genericPlan <- trGenericPlan c

    let (Int jointype) = fs ! "jointype"
    let (Bool inner_unique) = fs ! "inner_unique"
    joinquals <- extractListM "joinquals" trExpr fs

    let (Bool skip_mark_restore) = fs ! "skip_mark_restore"

    mergeclauses <- extractListM "mergeclauses" trExpr fs
    let mergeFamilies = extractIntList "mergeFamilies" fs
    let mergeCollations = extractIntList "mergeCollations" fs
    let mergeStrategies = extractIntList "mergeStrategies" fs
    let mergeNullsFirst = extractBoolList "mergeNullsFirst" fs

    return O.MERGEJOIN
            { O.genericPlan       = genericPlan
            , O.jointype          = jointype
            , O.inner_unique      = O.PgBool inner_unique
            , O.joinquals         = O.List joinquals
            , O.skip_mark_restore = O.PgBool skip_mark_restore
            , O.mergeclauses      = O.List mergeclauses
            , O.mergeFamilies     = O.PlainList mergeFamilies
            , O.mergeCollations   = O.PlainList mergeCollations
            , O.mergeStrategies   = O.PlainList mergeStrategies
            , O.mergeNullsFirst   = O.PlainList $ map O.PgBool mergeNullsFirst
            }


trPlan c@(R.Chunk "UNIQUE" fs)
  = do
    genericPlan <- trGenericPlan c

    let (Int numCols) = fs ! "numCols"
    let uniqColIdx    = extractIntList "uniqColIdx" fs
    let uniqOperators = extractIntList "uniqOperators" fs

    return O.UNIQUE
            { O.genericPlan   = genericPlan
            , O.numCols       = numCols
            , O.uniqColIdx    = O.PlainList uniqColIdx
            , O.uniqOperators = O.PlainList uniqOperators
            }

trPlan c@(R.Chunk "SUBQUERYSCAN" fs)
  = do
    genericPlan <- trGenericPlan c

    let (Int scanrelid) = fs ! "scanrelid"
    subplan <- extractVChunkM "subplan" trPlan fs

    return O.SUBQUERYSCAN
            { O.genericPlan = genericPlan
            , O.scanrelid   = scanrelid
            , O.subplan     = subplan
            }

trPlan c@(R.Chunk "FUNCTIONSCAN" fs)
  = do
    genericPlan <- trGenericPlan c

    let (Int scanrelid) = fs ! "scanrelid"
    functions <- extractListM "functions" trExpr fs
    let (Bool funcordinality) = fs ! "funcordinality"

    return O.FUNCTIONSCAN
            { O.genericPlan = genericPlan
            , O.scanrelid   = scanrelid
            , O.functions   = O.List functions
            , O.funcordinality = O.PgBool funcordinality
            }

trPlan c@(R.Chunk "CTESCAN" fs)
  = do
    genericPlan <- trGenericPlan c

    let (Int scanrelid) = fs ! "scanrelid"
    let (Int ctePlanId) = fs ! "ctePlanId"
    let (Int cteParam)  = fs ! "cteParam"

    return O.CTESCAN
            { O.genericPlan = genericPlan
            , O.scanrelid   = scanrelid
            , O.ctePlanId   = ctePlanId
            , O.cteParam    = cteParam
            }

trPlan c@(R.Chunk "GATHER" fs)
  = do
    genericPlan <- trGenericPlan c

    let (Int num_workers)  = fs ! "num_workers"
    let (Int rescan_param) = fs ! "rescan_param"
    let (Bool single_copy) = fs ! "single_copy"
    let (Bool invisible)   = fs ! "invisible"

    return O.GATHER
            { O.genericPlan  = genericPlan
            , O.num_workers  = num_workers
            , O.rescan_param = rescan_param
            , O.single_copy  = O.PgBool single_copy
            , O.invisible    = O.PgBool invisible
            }

trPlan c@(R.Chunk "GATHERMERGE" fs)
  = do
    genericPlan <- trGenericPlan c

    let (Int num_workers)  = fs ! "num_workers"
    let (Int rescan_param) = fs ! "rescan_param"
    let (Int numCols)      = fs ! "numCols"
    let sortColIdx         = extractIntList "sortColIdx" fs
    let sortOperators      = extractIntList "sortOperators" fs
    let collations         = extractIntList "collations" fs
    let nullsFirst         = extractBoolList "nullsFirst" fs

    return O.GATHERMERGE
            { O.genericPlan = genericPlan
            , O.num_workers  = num_workers
            , O.rescan_param = rescan_param
            , O.numCols       = numCols
            , O.sortColIdx    = O.PlainList sortColIdx
            , O.sortOperators = O.PlainList sortOperators
            , O.collations    = O.PlainList collations
            , O.nullsFirst    = O.PlainList $ map O.PgBool nullsFirst
            }

trPlan c@(R.Chunk "HASH" fs)
  = do
    genericPlan <- trGenericPlan c
    let (Int skewTable) = fs ! "skewTable"
    let (Int skewColumn) = fs ! "skewColumn"
    let (Bool skewInherit) = fs ! "skewInherit"

    return $ O.HASH
              { O.genericPlan = genericPlan
              , O.skewTable   = skewTable
              , O.skewColumn  = skewColumn
              , O.skewInherit = O.PgBool skewInherit
              }

trPlan c@(R.Chunk "HASHJOIN" fs)
  = do
    genericPlan <- trGenericPlan c
    let (Int jointype) = fs ! "jointype"
    let (Bool inner_unique) = fs ! "inner_unique"
    joinqual <- extractListM "joinqual" trExpr fs
    hashclauses <- extractListM "hashclauses" trExpr fs

    return $ O.HASHJOIN
              { O.genericPlan   = genericPlan
              , O.jointype      = jointype
              , O.inner_unique  = O.PgBool inner_unique
              , O.joinqual      = O.List joinqual
              , O.hashclauses   = O.List hashclauses
              }

trPlan c@(R.Chunk "SETOP" fs)
  = do
    genericPlan <- trGenericPlan c

    let (Int cmd)        = fs ! "cmd"
    let (Int strategy)   = fs ! "strategy"
    let (Int numCols)    = fs ! "numCols"
    let dupColIdx        = extractIntList "dupColIdx" fs
    let dupOperators     = extractIntList "dupOperators" fs
    let (Int flagColIdx) = fs ! "flagColIdx"
    let (Int firstFlag)  = fs ! "firstFlag"
    let (Int numGroups)  = fs ! "numGroups"
    
    return $ O.SETOP
              { O.genericPlan   = genericPlan
              , O.cmd           = cmd
              , O.strategy      = strategy
              , O.numCols       = numCols
              , O.dupColIdx     = O.PlainList dupColIdx
              , O.dupOperators  = O.PlainList dupOperators
              , O.flagColIdx    = flagColIdx
              , O.firstFlag     = firstFlag
              , O.numGroups     = numGroups
              }


trPlan (R.Chunk c _) = error $ "trPlan not implemented for: " ++ c

trNestParam :: Rule R.Chunk O.NestLoopParam
trNestParam c@(R.Chunk "NESTLOOPPARAM" fs)
  = do
    let (Int paramno) = fs ! "paramno"
    paramval <- extractVChunkM "paramval" trExpr fs

    return O.NESTLOOPPARAM
            { O.paramno = paramno
            , O.paramval = paramval
            }

trRangeEx :: Rule R.Chunk O.RangeEx
trRangeEx (R.Chunk "RTE" fs)
  = do
    alias <- extractMaybeVChunkM "alias" trAlias fs
    eref  <- extractVChunk "eref" trAlias fs
    let (Int rtekind) = fs ! "rtekind"
    let (Bool lateral) = fs ! "lateral"
    let (Bool inh)     = fs ! "inh"
    let (Bool inFromCl) = fs ! "inFromCl"
    let (Int requiredPerms) = fs ! "requiredPerms"
    let (Int checkAsUser)   = fs ! "checkAsUser"
    let selectedCols = extractIntList "selectedCols" fs
    let insertedCols = extractIntList "insertedCols" fs
    let updatedCols  = extractIntList "updatedCols" fs

    case rtekind of
      0 -> do 
          let (Int relid)   = fs ! "relid"
          let (Str relkind) = fs ! "relkind"
          return $ O.RTE
                  { O.alias         = alias
                  , O.eref          = eref
                  , O.rtekind       = rtekind
                  , O.relid         = relid
                  , O.relkind       = relkind
                  , O.tablesample   = O.Null
                  , O.lateral       = O.PgBool lateral
                  , O.inh           = O.PgBool inh
                  , O.inFromCl      = O.PgBool inFromCl
                  , O.requiredPerms = requiredPerms
                  , O.checkAsUser   = checkAsUser
                  , O.selectedCols  = O.Bitmapset selectedCols
                  , O.insertedCols  = O.Bitmapset insertedCols
                  , O.updatedCols   = O.Bitmapset updatedCols
                  , O.securityQuals = O.Null
                  }
      1 -> do
          let (Bool security_barrier) = fs ! "security_barrier"
          return $ O.RTE_SUBQUERY
                { O.alias            = alias
                , O.eref             = eref
                , O.rtekind          = rtekind
                , O.subquery         = O.Null
                , O.security_barrier = O.PgBool security_barrier
                , O.lateral          = O.PgBool lateral
                , O.inh              = O.PgBool inh
                , O.inFromCl         = O.PgBool inFromCl
                , O.requiredPerms    = requiredPerms
                , O.checkAsUser      = checkAsUser
                , O.selectedCols     = O.Bitmapset selectedCols
                , O.insertedCols     = O.Bitmapset insertedCols
                , O.updatedCols      = O.Bitmapset updatedCols
                , O.securityQuals    = O.Null
                }

      6 -> do
          let (Str ctename)         = fs ! "ctename"
          let (Int ctelevelsup)     = fs ! "ctelevelsup"
          let (Bool self_reference) = fs ! "self_reference"

          return $ O.RTE_CTE
                { O.alias          = alias
                , O.eref           = eref
                , O.rtekind        = rtekind
                , O.ctename        = ctename
                , O.ctelevelsup    = ctelevelsup
                , O.self_reference = O.PgBool self_reference
                , O.coltypes       = O.Null
                , O.coltypmods     = O.Null
                , O.colcollations  = O.Null
                , O.lateral        = O.PgBool lateral
                , O.inh            = O.PgBool inh
                , O.inFromCl       = O.PgBool inFromCl
                , O.requiredPerms  = requiredPerms
                , O.checkAsUser    = checkAsUser
                , O.selectedCols   = O.Bitmapset selectedCols
                , O.insertedCols   = O.Bitmapset insertedCols
                , O.updatedCols    = O.Bitmapset updatedCols
                , O.securityQuals  = O.Null
                }
      x -> error $ "rtekind " ++ show x ++ " not implemented."

trAlias :: Rule R.Chunk O.Alias
trAlias (R.Chunk "ALIAS" fs)
  = do
    let (Str aliasname) = fs ! "aliasname"
    -- aliasname <- fresh aliasname
    let (List colnames) = fs ! "colnames"
    let colnames = case fs ! "colnames" of
                    (List colnames) -> map (\(Str x) -> x) colnames
                    NoValue -> []
    -- colnames <- mapM fresh colnames
    return $ O.Alias
              { O.aliasname = aliasname
              , O.colnames  = O.List colnames
              }

trTargetEx :: Rule R.Chunk O.TARGETENTRY
trTargetEx (R.Chunk "TARGETENTRY" fs)
  = do
    expr <- extractVChunkM "expr" trExpr fs
    let (Int resno) = fs ! "resno"
    let resname' = extractMaybeStr "resname" fs
    -- resname <- do
    --   if isNothing resname'
    --   then fresh "freshName" >>= \x -> return $ Just x
    --   else return resname'
    let resname = resname'
    let (Int ressortgroupref) = fs ! "ressortgroupref"
    let (Int resorigtbl) = fs ! "resorigtbl"
    let (Int resorigcol) = fs ! "resorigcol"
    let (Bool resjunk)   = fs ! "resjunk"
    return $ O.TARGETENTRY
              { O.expr = expr
              , O.resno = resno
              , O.resname = resname
              , O.ressortgroupref = ressortgroupref
              , O.resorigtbl = resorigtbl
              , O.resorigcol = resorigcol
              , O.resjunk = O.PgBool resjunk
              }

trExpr :: Rule R.Chunk O.Expr
trExpr (R.Chunk "VAR" fs)
  = do
    let (Int varno)       = fs ! "varno"
    let (Int varattno)    = fs ! "varattno"
    let (Int vartype)     = fs ! "vartype"
    let (Int vartypmod)   = fs ! "vartypmod"
    let (Int varcollid)   = fs ! "varcollid"
    let (Int varlevelsup) = fs ! "varlevelsup"
    let (Int varnoold)    = fs ! "varnoold"
    let (Int varoattno)   = fs ! "varoattno"
    let (Int location)    = fs ! "location"
    return $ O.VAR 
              { O.varno       = varno
              , O.varattno    = varattno
              , O.vartype     = vartype
              , O.vartypmod   = vartypmod
              , O.varcollid   = varcollid
              , O.varlevelsup = varlevelsup
              , O.varnoold    = varnoold
              , O.varoattno   = varoattno
              , O.location    = location
              }

trExpr c@(R.Chunk "CONST" fs)
  = do
    let (Int consttype)    = fs ! "consttype"
    let (Int consttypmod)  = fs ! "consttypmod"
    let (Int constcollid)  = fs ! "constcollid"
    let (Int constlen)     = fs ! "constlen"
    let (Bool constbyval)  = fs ! "constbyval"
    let (Bool constisnull) = fs ! "constisnull"
    let constvalue = case fs ! "constvalue" of
                    (Extra (Int l) (Sequence constvalue')) -> Just $ O.Seq l constvalue'
                    NoValue -> Nothing
    let c' = O.CONST
              { O.consttype=consttype
              , O.consttypmod=consttypmod
              , O.constcollid=constcollid
              , O.constlen=constlen
              , O.constbyval=O.PgBool constbyval
              , O.constisnull=O.PgBool constisnull
              , O.location= -1
              , O.constvalue=constvalue
              }
    logConst $ (c', pr c)
    return c'


trExpr (R.Chunk "FUNCEXPR" fs)
  = do
    let (Int funcid)         = fs ! "funcid"
    let (Int funcresulttype) = fs ! "funcresulttype"
    let (Bool funcretset)    = fs ! "funcretset"
    let (Bool funcvariadic)  = fs ! "funcvariadic"
    let (Int funcformat)     = fs ! "funcformat"
    let (Int funccollid)     = fs ! "funccollid"
    let (Int inputcollid)    = fs ! "inputcollid"

    args <- extractListM "args" trExpr fs
    let (Int location) = fs ! "location"

    return $ O.FUNCEXPR
              { O.funcid         = funcid
              , O.funcresulttype = funcresulttype
              , O.funcretset     = O.PgBool funcretset
              , O.funcvariadic   = O.PgBool funcvariadic
              , O.funcformat     = funcformat
              , O.funccollid     = funccollid
              , O.inputcollid    = inputcollid
              , O.args           = O.List args
              , O.location       = location
              }

trExpr (R.Chunk "OPEXPR" fs)
  = do
    let (Int opno)         = fs ! "opno"
    let (Int opfuncid)     = fs ! "opfuncid"
    let (Int opresulttype) = fs ! "opresulttype"
    let (Bool opretset)    = fs ! "opretset"
    let (Int opcollid)     = fs ! "opcollid"
    let (Int inputcollid)  = fs ! "inputcollid"

    args <- extractListM "args" trExpr fs
    let (Int location) = fs ! "location"

    return $ O.OPEXPR
            { O.opno         = opno
            , O.opfuncid     = opfuncid
            , O.opresulttype = opresulttype
            , O.opretset     = O.PgBool opretset
            , O.opcollid     = opcollid
            , O.inputcollid  = inputcollid
            , O.args         = O.List args
            , O.location     = location
            }

trExpr (R.Chunk "AGGREF" fs)
  = do
    let (Int aggfnoid) = fs ! "aggfnoid"
    let (Int aggtype)  = fs ! "aggtype"
    let (Int aggcollid) = fs ! "aggcollid"
    let (Int inputcollid) = fs ! "inputcollid"
    let (Int aggtranstype) = fs ! "aggtranstype"
    let aggargtypes = extractIntList "aggargtypes" fs
    aggdirectargs <- extractListM "aggdirectargs" trExpr fs
    args <- extractListM "args" trTargetEx fs
    aggorder <- extractListM "aggorder" trSORTGROUPCLAUSE fs
    aggdistinct <- extractListM "aggdistinct" trSORTGROUPCLAUSE fs
    aggfilter <- extractMaybeVChunkM "aggfilter" trExpr fs
    let (Bool aggstar) = fs ! "aggstar"
    let (Bool aggvariadic) = fs ! "aggvariadic"
    let (Str aggkind)      = fs ! "aggkind"
    let (Int agglevelsup)  = fs ! "agglevelsup"
    let (Int aggsplit)     = fs ! "aggsplit"
    let (Int location)     = fs ! "location"

    return O.AGGREF
            { O.aggfnoid = aggfnoid
            , O.aggtype  = aggtype
            , O.aggcollid = aggcollid
            , O.inputcollid = inputcollid
            , O.aggtranstype = aggtranstype
            , O.aggargtypes = O.RelationList aggargtypes
            , O.aggdirectargs = O.List aggdirectargs
            , O._args = O.List args
            , O.aggorder = O.List aggorder
            , O.aggdistinct = O.List aggdistinct
            , O.aggfilter = aggfilter
            , O.aggstar = O.PgBool aggstar
            , O.aggvariadic = O.PgBool aggvariadic
            , O.aggkind = aggkind
            , O.agglevelsup = agglevelsup
            , O._aggsplit = aggsplit
            , O.location = location
            }
 
trExpr (R.Chunk "SUBPLAN" fs)
  = do
    let (Int subLinkType) = fs ! "subLinkType"
    testexpr <- extractMaybeVChunkM "testexpr" trExpr fs
    let paramIds = extractIntList "paramIds" fs
    let (Int plan_id) = fs ! "plan_id"
    let (Str plan_name) = fs ! "plan_name"
    let (Int firstColType) = fs ! "firstColType"
    let (Int firstColTypmod) = fs ! "firstColTypmod"
    let (Int firstColCollation) = fs ! "firstColCollation"
    let (Bool useHashTable)     = fs ! "useHashTable"
    let (Bool unknownEqFalse)   = fs ! "unknownEqFalse"
    let (Bool _parallel_safe)   = fs ! "parallel_safe"
    let setParam = extractIntList "setParam" fs
    let parParam = extractIntList "parParam" fs
    args <- extractListM "args" trExpr fs
    let (Dbl startup_cost) = fs ! "startup_cost"
    let (Dbl per_call_cost) = fs ! "per_call_cost"

    return O.SUBPLAN
            { O.subLinkType = subLinkType
            , O.testexpr    = testexpr
            , O.paramIds    = O.List paramIds
            , O.plan_id     = plan_id
            , O.plan_name   = plan_name
            , O.firstColType = firstColType
            , O.firstColTypmod = firstColTypmod
            , O.firstColCollation = firstColCollation
            , O.useHashTable = O.PgBool useHashTable
            , O.unknownEqFalse = O.PgBool unknownEqFalse
            , O._parallel_safe = O.PgBool _parallel_safe
            , O.setParam = O.IndexList setParam
            , O.parParam = O.IndexList parParam
            , O.args = O.List args
            , O._startup_cost = startup_cost
            , O.per_call_cost = per_call_cost
            }

trExpr (R.Chunk "PARAM" fs)
  = do
    let (Int paramkind) = fs ! "paramkind"
    let (Int paramid)   = fs ! "paramid"
    let (Int paramtype) = fs ! "paramtype"
    let (Int paramtypmod) = fs ! "paramtypmod"
    let (Int paramcollid) = fs ! "paramcollid"
    let (Int location)    = fs ! "location"

    return O.PARAM
            { O.paramkind   = paramkind
            , O.paramid     = paramid
            , O.paramtype   = paramtype
            , O.paramtypmod = paramtypmod
            , O.paramcollid = paramcollid
            , O.location    = location }

trExpr (R.Chunk "BOOLEXPR" fs)
  = do
    let (Str boolop) = fs ! "boolop"
    args <- extractListM "args" trExpr fs
    let (Int location) = fs ! "location"

    return $ O.BOOLEXPR
              { O.boolop   = boolop
              , O.args     = O.List args
              , O.location = location }

trExpr (R.Chunk "NULLTEST" fs)
  = do
    arg <- extractVChunkM "arg" trExpr fs
    let (Int nulltesttype) = fs ! "nulltesttype"
    let (Bool argisrow)    = fs ! "argisrow"
    let (Int location)     = fs ! "location"

    return $ O.NULLTEST
              { O.arg = arg
              , O.nulltesttype = nulltesttype
              , O.argisrow     = O.PgBool argisrow
              , O.location     = location
              }

trExpr (R.Chunk "RELABELTYPE" fs)
  = do
    let (R.VChunk expr) = fs ! "arg"
    trExpr expr

trExpr (R.Chunk "COERCEVIAIO" fs)
  = do
    let (R.VChunk expr) = fs ! "arg"
    trExpr expr

trExpr (R.Chunk c _) = error $ "trExpr not implemented for: " ++ c

trSORTGROUPCLAUSE :: Rule R.Chunk O.SORTGROUPCLAUSE
trSORTGROUPCLAUSE (R.Chunk "SORTGROUPCLAUSE" fs)
  = do
    let (Int tleSortGroupRef) = fs ! "tleSortGroupRef"
    let (Int eqop) = fs ! "eqop"
    let (Int sortop) = fs ! "sortop"
    let (Bool nulls_first) = fs ! "nulls_first"
    let (Bool hashable)    = fs ! "hashable"

    return O.SORTGROUPCLAUSE
            { O.tleSortGroupRef = tleSortGroupRef
            , O.eqop            = eqop
            , O.sortop          = sortop
            , O.nulls_first     = O.PgBool nulls_first
            , O.hashable        = O.PgBool hashable }
  
extractMaybeStr :: String -> M.Map String Value -> Maybe String
extractMaybeStr s fs = case M.lookup s fs of
                        Just (Str x) -> Just x
                        Just NoValue -> Nothing
                        Just err     -> error $ "Tried to extract string but found " ++ (show err)
                        Nothing      -> Nothing

extractMaybeInt :: String -> M.Map String Value -> Maybe Integer
extractMaybeInt s fs = case M.lookup s fs of
                        Just (Int x) -> Just x
                        Just NoValue -> Nothing
                        Just err     -> error $ "Tried to extract Int but found " ++ (show err)
                        Nothing      -> Nothing

-- extractListM :: Rule (String, (Rule Chunk a), M.Map String Value) [a]
extractListM :: Rule3 String (Rule Chunk a) (M.Map String Value) [a]
extractListM s f fs = case M.lookup s fs of
                            Just (List x) -> mapM (\p -> extractTestM $ (f, p)) x --(\ (VChunk t) -> f t) x
                            Just NoValue  -> return []
                            Just _        -> error $ "extractList " ++ s
                            Nothing       -> return []

extractTestM :: Rule (Rule Chunk a, Value) a
extractTestM (f, (VChunk t)) = f t
extractTestM (_, err) = error $ "Expected list of VChunks but found: " ++ (show err)


extractIntList :: String -> M.Map String Value -> [Integer]
extractIntList s fs = case fs ! s of
                      (Int x) -> [x]
                      (List x) -> case x of
                                    [] -> []
                                    (Int _):_ -> map (\(Int y) -> y) x
                                    _         -> map (\(Int y) -> y) $ tail x
                      (NoValue) -> []
                      _        -> error $ "extractIntList " ++ s

extractBoolList :: String -> M.Map String Value -> [Bool]
extractBoolList s fs = case fs ! s of
                      (Bool x) -> [x]
                      (List x) -> case x of 
                                    [] -> []
                                    (Bool _):_ -> map (\(Bool y) -> y) x
                                    _          -> map (\(Bool y) -> y) $ tail x
                      (NoValue) -> []
                      _        -> error $ "extractBoolList " ++ s


extractList :: String -> (Chunk -> a) -> M.Map String Value -> [a]
extractList s f fs = case fs ! s of
                    (List x) -> map (extractTest f) x --(\ (VChunk t) -> f t) x
                    NoValue  -> []
                    _        -> error $ "extractList " ++ s

extractTest :: (Chunk -> a) -> Value -> a
extractTest f (VChunk t) = f t
extractTest _ err = error $ "Expected list of VChunks but found: " ++ (show err)

extractVChunk :: String -> (Chunk -> a) -> M.Map String Value -> a
extractVChunk s f fs = case fs ! s of
                        (VChunk x) -> f x
                        err        -> error $ "Tried to extract VChunk but found " ++ (show err)

extractVChunkM :: Rule3 String (Rule Chunk a) (M.Map String Value) a
extractVChunkM s f fs = case fs ! s of
                        (VChunk x) -> f x
                        err        -> error $ "Tried to extract VChunk but found " ++ (show err)

extractMaybeVChunkM :: Rule3 String (Rule Chunk a) (M.Map String Value) (Maybe a)
extractMaybeVChunkM s f fs = case fs ! s of
                            (VChunk x) -> do
                                        x' <- f x
                                        return $ Just x'
                            _          -> return Nothing


extractMaybeVChunk :: String -> (Chunk -> a) -> M.Map String Value -> Maybe a
extractMaybeVChunk s f fs = case fs ! s of
                            (VChunk x) -> Just $ f x
                            _          -> Nothing

extractLocation :: M.Map String Value -> Maybe Integer
extractLocation fs = case M.lookup "location" fs of
                        Just (Int (-1)) -> Nothing
                        Just (Int x)  -> Just x
                        _             -> Nothing
