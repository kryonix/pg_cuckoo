{-# LANGUAGE NamedFieldPuns #-}

module PAtoRA ( parse, fixVarP, fixVar ) where

import OperSemIO
import qualified Database.PgCuckoo.PgPlan as O
import qualified LogicalAST as A
import qualified RAUtils as U

import Database.PgCuckoo.GetTable as Tbl

import Data.Maybe

import qualified Text.Show.Pretty as PP

import qualified Data.Map as M
import qualified Data.List as DL

import Data.Stack as S

(!!) :: [a] -> Int -> Maybe a
(!!) lst idx = if idx >= length lst
                then Nothing
                else Just $ lst Prelude.!! idx

--------------------------------------------------------------------------------
-- WRITER MONAD SECTION
data Log = Log { errors :: [ String ] }

instance Monoid Log where
  mempty = Log []
  (Log e1) `mappend` (Log e2) = Log (e1++e2)

logError :: Rule String ()
logError err = tell (Log [err])

--------------------------------------------------------------------------------
-- READER MONAD SECTION
data Context = C { tableData  :: TableData
                 , consts     :: [(O.Expr, String)]
                 , rtable     :: [O.RangeEx]
                 , subplans   :: [O.Plan]
                 , parameters :: [ (Integer, A.Expr) ]
                 , tarL       :: [ (Integer, String) ]
                 , tarR       :: [ (Integer, String) ]
                 }

mkContext :: Rule2 (Maybe A.Plan) (Maybe A.Plan) Context
mkContext tL tR
  = do
    c <- lift ask
    let prodL = case tL of
                Nothing -> []
                Just x  -> prodVars x
    let prodR = case tR of
                  Nothing -> []
                  Just x  -> prodVars x
    let c' = c { tarL = prodL, tarR = prodR}
    return $ c'

getRTableData :: Rule () TableData
getRTableData () = lift $ asks tableData

getTarL :: Rule () [ (Integer, String) ]
getTarL () = lift $ asks tarL

getTarR :: Rule () [ (Integer, String) ]
getTarR () = lift $ asks tarR

getRTableRow :: Rule (TableData -> Tbl.Table, Table -> [Row]) [Row]
getRTableRow (tbl, idx) = do
    td <- getRTableData ()
    let ops = tbl td
    let row = idx ops
    return row

getConstValue :: Rule O.Expr (Maybe String)
getConstValue e
  = do
    c <- lift $ asks consts
    let c' = filter ((==e) . fst) c
    case c' of
      [] -> return Nothing
      x:_ -> return (Just (snd x))

getRTableEntry :: Rule Int (Maybe O.RangeEx)
getRTableEntry i
  = do
    c <- lift $ asks rtable
    return $ c PAtoRA.!! i

--------------------------------------------------------------------------------
-- STATE MONAD SECTION

type StackElem = [A.Expr] -> A.Plan -> A.Plan

data StateI = StateI
              { stack :: S.Stack StackElem }

instance Monoid StateI where
  mempty = StateI { stack = S.stackNew }
  _ `mappend` b = b

stPush :: Rule StackElem ()
stPush op = do
  st <- lift $ gets stack
  lift $ modify (\x -> x { stack = stackPush st op })
  return ()

stPop :: Rule () (Maybe StackElem)
stPop () = do
  st <- lift $ gets stack
  let sp = stackPop st
  case sp of
    Nothing -> return Nothing
    Just (st', e) -> do
                      lift $ modify (\x -> x { stack = st' })
                      return $ Just e

stPeek :: Rule () (Maybe StackElem)
stPeek () = do
  st <- lift $ gets stack
  let sp = stackPeek st
  return sp

stStackIsEmpty :: Rule () Bool
stStackIsEmpty () = do
  st <- lift $ gets stack
  return $ stackIsEmpty st

--------------------------------------------------------------------------------

type Continuation = (Maybe (A.Plan -> A.Plan), Maybe A.Expr)

type Rule a b = a -> OperSemIO StateI Context b Log

-- | Two parameter rule
type Rule2 a b c = a -> b -> OperSemIO StateI Context c Log

-- | Three parameter rule
type Rule3 a b c d = a -> b -> c -> OperSemIO StateI Context d Log

-- | Validates an operator and returns all errors
parse :: TableData -> O.PLANNEDSTMT -> [(O.Expr, String)] -> IO A.Plan
parse e op c = do
              ((erg, lg), state) <- runOperSemSTIO ((+>) op) mempty (C e c [] (unList $ O.subplans op) [] [] [])
              let (Log lg') = lg
              let res = case erg of
                        Prelude.Left str -> error $ "Inference error: " ++ str
                        Prelude.Right a -> a
              let stStack = stack state
              unless (stackIsEmpty stStack) $ liftIO $ putStrLn "WARNING: Stack is not empty!"
              return res

(!) :: (Show a, Ord a) => M.Map a b -> a -> b
fs ! key = fromMaybe
            (error $ "given key " ++ PP.ppShow key ++ " is not an element in the map")
            (M.lookup key fs)

unList :: O.List a -> [a]
unList (O.List xs) = xs

unPList :: O.PlainList a -> [a]
unPList (O.PlainList xs) = xs

unPgBool :: O.PgBool -> Bool
unPgBool (O.PgBool b) = b

--------------------------------------------------------------------------------
-- PostgreSQL uses VAR expressions to reference base tables, indexes and sub-plans.
-- Relational algebra on the other hand is more restricted.
-- This set of functions tries to replace general VAR expressions by
-- VAR sub expressions which strictly reference a column produced by the sub-plan.

fixVarP :: Monad m => A.Plan -> m A.Plan
fixVarP (A.Selection p lt)
  = do
    lt' <- fixVarP lt
    let prodV = map snd $ prodVars lt'
    p' <- mapM (fixVar prodV []) p
    return $ A.Selection p' lt'

fixVarP (A.Join lt rt p t)
  = do
    lt' <- fixVarP lt
    rt' <- fixVarP rt
    p' <- mapM (fixVar (map snd $ prodVars lt) (map snd $ prodVars rt)) p
    return $ A.Join lt' rt' p' t

fixVarP j@(A.DependentJoin {A.leftTree=lt, A.rightTree=rt, A.predicate=p, A.joinType = t})
  = do
    lt' <- fixVarP lt
    rt' <- fixVarP rt
    p' <- mapM (fixVar (map snd $ prodVars lt) (map snd $ prodVars rt)) p
    return $ j { A.leftTree = lt'
                             , A.rightTree = rt'
                             , A.predicate = p'
                             , A.joinType = t}

fixVarP (A.Projection tl lt)
  = do
    lt' <- fixVarP lt
    let prodV = map snd $ prodVars lt'
    tl' <- mapM (A.mapTargetExpr (fixVar prodV [])) tl
    return $ A.Projection tl' lt'

fixVarP (A.GroupBy lt gc tl)
  = do
    lt' <- fixVarP lt
    let prodV = map snd $  prodVars lt'
    tl' <- mapM (A.mapTargetExpr (fixVar prodV [])) tl
    return $ A.GroupBy lt' gc tl'

fixVarP (A.Map lt tl)
  = do
    lt' <- fixVarP lt
    let prodV = map snd $ prodVars lt'
    tl' <- mapM (A.mapTargetExpr (fixVar prodV [])) tl
    return $ A.Map lt' tl'

fixVarP (A.Sort sc lt)
  = do
    lt' <- fixVarP lt
    let prodV = map snd $ prodVars lt'
    sc' <- mapM (fixVarSortEx prodV []) sc
    return $ A.Sort sc' lt'

fixVarP p = A.mapPlan fixVarP p

fixVarSortEx :: Monad m => [String] -> [String] -> A.SortEx -> m A.SortEx
fixVarSortEx pvL pvR e@(A.SortEx { A.sortTarget })
  = do
    sortTarget' <- fixVar pvL pvR sortTarget
    return $ e { A.sortTarget = sortTarget' }

fixVar :: Monad m => [String] -> [String] -> A.Expr -> m A.Expr
fixVar pvL pvR (A.VAR tn cn)
  = do
    case (cn `elem` pvL, cn `elem` pvR) of
      (True, False) -> return (A.VAR "l" cn)
      (False, True) -> return (A.VAR "r" cn)
      _             -> return (A.VAR tn cn)

fixVar pvL pvR e = A.mapExpr (fixVar pvL pvR) e

--------------------------------------------------------------------------------

(+>) :: Rule O.PLANNEDSTMT A.Plan
(+>) (O.PLANNEDSTMT { O.planTree, O.rtable })
  = do
    let rtable' = unList rtable
    context <- lift $ ask
    let context' = const $ context { rtable = rtable' }
    planTree' <- local context' $ trPlan planTree
    return $ planTree'

trPlan :: Rule O.Plan A.Plan
trPlan (O.RESULT { O.genericPlan , O.resconstantqual })
  = mkContext Nothing Nothing >>= \x -> local (const x) $
    do
    targetlist' <- trTargetlist genericPlan
    resconstantqual' <- mapM trExpr resconstantqual

    lefttree <- extractAndTR genericPlan O.lefttree trPlan

    case lefttree of
      Nothing -> do
                let con = A.Constant { A.targetList = targetlist' }
                return $ mkSelection con (maybeToList resconstantqual')
      Just x  -> do
                  if null targetlist'
                    then return $ mkSelection x (maybeToList resconstantqual')
                    else return A.Projection { A.targetList = targetlist', A.leftTree = mkSelection x (maybeToList resconstantqual') }

trPlan a@(O.SEQSCAN { O.genericPlan, O.scanrelid })
  = mkContext Nothing Nothing >>= \x -> local (const x) $
    do
    rte <- getRTableEntry $ fromIntegral (scanrelid-1) -- Postgres counts from 1!

    unless (isJust rte) $ error "SEQSCAN scanrelid not found in rtable"
    let rte' = fromJust rte
    (relname, colnames) <- accessPgAttribute (O.relid rte')

    let eref = O.eref rte'   -- expanded reference names
    let alias = O.alias rte' -- user-written alias clause, if any

    targetlist' <- trTargetlist genericPlan
    -- Compile qualification clauses
    let (qualDep, qualUnDep) = DL.partition U.hasSubPlan ((unList . O.qual) genericPlan)
    -- qual'' <- extractAndTR genericPlan (unList . O.qual) trExpr

    qual'' <- mapM trExpr qualDep

    -- If there is any dependent join present, it is now on the stack
    dependentJoin' <- stPop ()

    qualUnDep' <- mapM trExpr qualUnDep

    let (dependentJoin, qual') =
          case dependentJoin' of
            Nothing -> (Nothing, qualUnDep')
            Just x  -> (Just $ x qual'', qualUnDep')

    -- SeqScan always scans a relation
    let scan = return A.Scan
              { A.relation = relname
              , A.alias = O.aliasname eref
              , A.colnames = unList $ O.colnames eref }

    -- IFF alias is defines generate Rename node
    let rename x = do
                  case alias of
                    Nothing -> return x
                    Just alias' -> do
                      if (null $ ((unList . O.colnames) alias') DL.\\ colnames)
                        then return x
                        else return $ A.Rename
                                      { A.rename = zip colnames ((unList . O.colnames) alias')
                                      , A.leftTree = x }

    -- Generate Projection iff not all target list entries are VAR (plain relation accesses)
    let project x = do
                      if (all (\x -> case x of
                                      (A.TargetExpr { A.expr = A.VAR {} }) -> True
                                      _      -> False) targetlist')
                      then return x
                      else return A.Projection
                                  { A.targetList = targetlist'
                                  , A.leftTree = x }

    -- Generate Selection node if qual exists
    let selection x = return $ mkSelection x qual'

    case dependentJoin of
      Nothing -> scan >>= (rename >=> selection >=> project)
      Just x  -> do
                  p <- scan >>= (rename >=> selection >=> project)
                  return $ x p

trPlan (O.SORT { O.genericPlan, O.sortColIdx, O.sortOperators, O.nullsFirst })
  = do
    (Just lefttree'') <- extractAndTR genericPlan O.lefttree trPlan
    
    c' <- mkContext (Just lefttree'') Nothing

    targetlist' <- local (const c') $ trTargetlist genericPlan

    let zipped = zip3 (map (\x -> fromIntegral x-1) $ unPList sortColIdx) (unPList sortOperators) (unPList nullsFirst)
    
    zipped' <- mapM (\(a, b, c) -> do {b' <- getOperatorData b; return (A.expr $ targetlist' DL.!! (a), b', c)}) zipped
    let sortCols = map (\(e, op, b) -> A.SortEx e (op == "<") (unPgBool b)) zipped'

    return $ A.Sort
            { A.sortCols = sortCols
            , A.leftTree = lefttree'' }

trPlan (O.LIMIT { O.genericPlan, O.limitOffset, O.limitCount})
  = do
    let targetlist = unList $ O.targetlist genericPlan
    -- targetlist' <- mapM trTargetEntry targetlist
    (Just lefttree'') <- extractAndTR genericPlan O.lefttree trPlan
    c' <- mkContext (Just lefttree'') Nothing

    offset <- local (const c') $ mapM trExpr limitOffset
    count  <- local (const c') $ mapM trExpr limitCount

    return $ A.Limit
              { A.leftTree    = lefttree''
              , A.limitOffset = offset
              , A.limitCount  = count
              }

trPlan (O.AGG { O.genericPlan, O.grpColIdx })
  = do
    (Just lefttree') <- extractAndTR genericPlan O.lefttree trPlan
    c' <- mkContext (Just lefttree') Nothing
    let prodCols = tarL c'

    let targetlist = unList $ O.targetlist genericPlan

    -- Something is weird about grpColIdx, sometimes the referenced column does not exist
    let grpColIdx' = unPList grpColIdx

    let grpCols = filter (\x -> (O.resno x) `elem` grpColIdx') targetlist
    
    targetlist'' <- local (const c') $ trTargetlist genericPlan
    let grpCols' = map (\x -> prodCols DL.!! fromIntegral (x-1) ) grpColIdx'
--    grpCols'     <- mapM ((liftM A.expr) . trTargetEntry) grpCols
    
    -- HAVING
    let predicate = unList $ O.qual genericPlan

    -- Split having predicates into dependent and undependent expressions
    let (predicateDep, predicateUnDep) = DL.partition U.hasSubPlan predicate

    -- Translate dependent expressions
    predicate' <- mapM trExpr predicateDep
    -- We should now have the continuation of a dependent join on the stack
    p <- stPeek ()

    -- Translate undependent expressions
    predicateUnDep' <- mapM trExpr predicateUnDep

    -- Create selection node for undependent expressions
    let selection x = mkSelection x predicateUnDep'

    -- Create main group by node for the aggregation
    let groupByOp = A.GroupBy
                    { A.leftTree = lefttree'
                    , A.groupCols = map snd grpCols' -- grpColIdx' -- grpCols'
                    , A.targetList = targetlist''
                    }

    let project x = do
                  A.Projection
                              { A.targetList = 
                                [ A.TargetExpr
                                  { A.expr = A.VAR "l" (fromJust resname)
                                  , A.resname = resname
                                  , A.resno = resno }
                                | (A.TargetExpr {A.resname, A.resno}) <- targetlist''
                                ]
                              , A.leftTree = x }

    case p of
      Nothing -> return $ project $ selection groupByOp
      Just x  -> do
        stPop ()
        return $ project $ selection $ x predicate' groupByOp

trPlan (O.HASHJOIN { O.genericPlan
                   , O.jointype
                   , O.hashclauses = O.List hashclauses''
                   , O.joinqual = O.List joinqual'
                   , O.inner_unique = O.PgBool inner})
  = trJoin (genericPlan, jointype, hashclauses''++joinqual', inner)

{-
    T --> T'
--------------
HASH(T) --> T'
-}
trPlan (O.HASH { O.genericPlan })
  = do
    (Just lefttree) <- extractAndTR genericPlan O.lefttree trPlan
    return lefttree

trPlan (O.NESTLOOP { O.genericPlan
                   , O.jointype
                   , O.joinquals
                   , O.nestParams
                   , O.inner_unique })
  = trJoin (genericPlan, jointype, unList joinquals, unPgBool inner_unique)

trPlan (O.MATERIAL { O.genericPlan })
  = do
    (Just lefttree) <- extractAndTR genericPlan O.lefttree trPlan
    return lefttree

trPlan (O.GATHER { O.genericPlan })
  = do
    (Just lefttree) <- extractAndTR genericPlan O.lefttree trPlan
    return lefttree

trPlan err = error $ "Not implemented: " ++ PP.ppShow err

trJoin :: Rule (O.GenericPlan, Integer, [O.Expr], Bool) A.Plan
trJoin (genericPlan, jointype, joinquals, innerunique)
  = do
    lefttree'  <- extractAndTR genericPlan O.lefttree trPlan
    righttree' <- extractAndTR genericPlan O.righttree trPlan
    c' <- mkContext lefttree' righttree'

    targets' <- local (const c') $ trTargetlist genericPlan

    let proj x = A.Projection
                  { A.targetList = targets'
                  , A.leftTree = x }

    -- A hashjoin has two fields for join qualifications
    -- RA does not require to differentiate these, so we
    -- just use the union of the expressions.
    let hashclauses' = joinquals
    -- We consider a join qualification as dependent, if it contains a subplan.
    let (hashclausesDep, hashclausesUnDep) = DL.partition U.hasSubPlan hashclauses'

    -- Compile dependent hashclauses
    hashclauses'' <- local (const c') $ mapM trExpr hashclausesDep
    -- There should be a dependent join on the stack now.
    p <- stPeek ()

    -- Compile regular hashclauses
    hashclausesUnDep' <- local (const c') $ mapM trExpr hashclausesUnDep

    let jointype' = U.trJoinType jointype

    -- A' ⨝{non dependent quals} B'
    let result = A.Join
                { A.leftTree  = fromJust lefttree'
                , A.rightTree = fromJust righttree'
                , A.predicate = hashclausesUnDep'
                , A.joinType  = jointype'
                }
    case p of
      Nothing -> return $ proj $ result
      -- (A' ⨝{p} B') ⧑{dependent quals} C
      Just depJoin -> do
                      stPop ()
                      return $ proj $ depJoin hashclauses'' result

--------------------------------------------------------------------------------
-- trPlan helper functions

extractAndTR :: (Traversable t, Monad m) => p -> (p -> t a) -> (a -> m b) -> (m (t b))
extractAndTR genericPlan selector f
  = do
    let el = selector genericPlan
    mapM f el

trTargetlist :: Rule O.GenericPlan [A.TargetExpr]
trTargetlist genericPlan = extractAndTR genericPlan (unList . O.targetlist) trTargetEntry

mkSelection :: A.Plan -> [A.Expr] -> A.Plan
mkSelection p [] = p
mkSelection p q  = A.Selection { A.predicate = q, A.leftTree = p }

--------------------------------------------------------------------------------

trTargetEntry :: Rule O.TARGETENTRY A.TargetExpr
trTargetEntry (O.TARGETENTRY { O.expr, O.resname = resname@(Just _), O.resno })
  = do
    expr' <- trExpr expr

    stDJ <- stPop ()

    -- IF a target entry generates a dependent join, we have to change the
    -- join type to LEFT, because when the subplan generates no result,
    -- a NULL value is inserted. We have to mimic this behavior.
    case stDJ of
      Nothing -> return ()
      Just x  -> do
                  stPush (\a b -> (x a b) { A.joinType = A.LEFT })
                  return ()

    return $ A.TargetExpr
              { A.expr = expr'
              , A.resname = resname
              , A.resno   = resno }

trTargetEntry c@(O.TARGETENTRY { O.expr, O.resname = Nothing, O.resno })
  = do
    expr' <- trExpr expr
    let resname = case expr' of
                (A.VAR {A.colName}) -> Just colName
                (A.VARSUB {A.colName}) -> Just colName
                (A.DEP {A.arg = A.VAR {A.colName}}) -> Just colName
                (A.AGG {A.aggname}) -> Just aggname
                e -> error $ "failed to infer name " ++ show c -- Nothing
    return $ A.TargetExpr
              { A.expr    = expr'
              , A.resname = resname
              , A.resno   = resno }

trExpr :: Rule O.Expr A.Expr
-- Access to lefttree target list
-- We 'abuse' varnoold in order to dereference the column which this
-- expression references. This is not a stable method, because this field
-- is invalid in many cases.
-- If we encounter this situation, we use the context, which should contain
-- schema information of the corresponding sub-plan.
trExpr v@(O.VAR { O.varno = 65001, O.varnoold, O.varoattno, O.varattno })
  = do
    tarL <- getTarL ()
    if varnoold == 0 -- && False -- || True
    then do
      let v' = case lookup varattno tarL of
                Just x -> A.VAR "l" x
                Nothing -> error "VAR lookup not successful"

      return v' -- $ A.VAR "l" (tarL Prelude.!! (fromIntegral varattno-1))
    else do
      when (varnoold == 0) $ error $ "trExpr O.VAR 65001 error: varnoold is 0" ++ show tarL ++ "\n" ++ show v

      rte <- getRTableEntry $ fromIntegral (varnoold-1) -- Postgres counts from 1!

      unless (isJust rte) $ error "VAR varno not found in rtable"
      let rte' = fromJust rte
      (relname, colnames) <- accessPgAttribute (O.relid rte')
      let colnames' = unList $ O.colnames $ O.eref rte'
      let relname'  = O.aliasname $ O.eref rte'

      let varoattno' = if varoattno == 0 then varattno else varoattno
      return $ A.VAR
                { A.tableName = relname'
                , A.colName   = colnames' Prelude.!! (fromIntegral varoattno'-1) }

trExpr p@(O.VAR { O.varno = 65000, O.varnoold, O.varoattno, O.varattno })
  = do
    tarR <- getTarR ()
    if varnoold == 0 && False -- Something weird going on here
    then do
      let v' = case lookup varattno tarR of
                Just x -> A.VAR "r" x
                Nothing -> error "VAR lookup not successful"

      return v' -- $ A.VAR "r" (tarR Prelude.!! (fromIntegral varattno))
    else do
      when (varnoold == 0) $ error "trExpr O.VAR 65000 error: varnoold is 0"

      rte <- getRTableEntry $ fromIntegral (varnoold-1) -- Postgres counts from 1!

      unless (isJust rte) $ error "VAR varno not found in rtable"
      let rte' = fromJust rte
      (relname, colnames) <- accessPgAttribute (O.relid rte')
      let colnames' = unList $ O.colnames $ O.eref rte'
      let relname'  = O.aliasname $ O.eref rte'

      let varoattno' = if varoattno == 0 then varattno else varoattno
      return $ A.VAR
                { A.tableName = relname'
                , A.colName   = colnames' Prelude.!! (fromIntegral varoattno'-1) }


trExpr (O.VAR { O.varno, O.varattno })
  = do
    rte <- getRTableEntry $ fromIntegral (varno-1) -- Postgres counts from 1!

    unless (isJust rte) $ error $ "VAR varno not found in rtable: " ++ show varno
    let rte' = fromJust rte
    (relname, colnames) <- accessPgAttribute (O.relid rte')
    let relname'  = O.aliasname $ O.eref rte'
    let colnames  = unList $ O.colnames $ O.eref rte'

    return $ A.VAR
              { A.tableName = relname'
              , A.colName   = colnames Prelude.!! (fromIntegral varattno-1) }

trExpr c@(O.CONST { O.consttype })
  = do
    c' <- getConstValue c
    typ <- trType consttype
    case c' of
      Just x -> return $ A.Const
                          { A.value = x
                          , A.typ   = typ }
      Nothing -> error $ "Const not found"

trExpr (O.OPEXPR { O.opno, O.args })
  = do
    args' <- mapM trExpr $ unList args

    opname <- getOperatorData opno
    return $ A.Operator
              { A.operator = opname
              , A.args     = args' }

    -- return $ (head args''', Just $ A.Operator
    --           { A.operator = opname
    --           , A.args = args'' })

trExpr (O.FUNCEXPR { O.funcid, O.args = O.List args' })
  = do
    args'' <- mapM trExpr args'
    fname <- getProcData funcid

    return $ A.Function
              { A.function = fname
              , A.args     = args'' }

-- Kind of works, but the generated Plan needs a predicate,
-- which has to be annotated from 'above' in the tree.
-- Continuation does not help very much here.
trExpr (O.SUBPLAN {O.plan_id, O.testexpr, O.subLinkType, O.parParam, O.args})
  = do
    -- Prepare SUBPLAN args
    -- Bound variables from INNER PLAN to be used by the OUTER PLAN of the DJ
    let args'' = unList args
    args'' <- mapM trExpr args''
    let args' = [ A.DEP x | x <- args'' ]
    let (O.IndexList p) = parParam

    c <- lift $ ask
    let subplan = subplans c
    let (Just plan) = subplan PAtoRA.!! (fromIntegral plan_id-1)

    let c' = c { parameters = (zip p args') ++ (parameters c)}

    testexpr' <- mapM trExpr testexpr

    let cp' = case subLinkType of
                0 -> \x -> A.GroupBy x 
                            []
                            [ A.TargetExpr 
                              (A.Operator ">" [ A.AGG "count" [] True, A.Const "0" "int4"])
                              (Just "exists")
                              1
                            ]
                2 -> \x -> A.Projection
                            [ A.TargetExpr
                              (fromJust testexpr')
                              (Just "any")
                              1
                            ]
                            x
                _ -> \x -> x

    plan'' <- local (const c') $ trPlan plan
    let plan' = cp' plan''
    -- liftIO $ putStrLn $ PP.ppShow plan'
    let tlH = A.resname $ head $ A.targetList $ plan'
    let tl = A.VAR "r" (fromJust tlH) --A.expr $ head $ A.targetList plan'

    -- Outer plan and predicate is a continuation.
    -- Inner plan is plan' (aka the corresponding SUBPLAN)
    -- plan' is lifted from PLANNEDSTMT into the dependent join
    stPush (\prd x -> A.DependentJoin { A.leftTree = x
                                    , A.rightTree = plan'
                                    , A.predicate = prd
                                    , A.binding = zip p args''
                                    , A.joinType = A.INNER } )

    return tl

trExpr (O.PARAM {O.paramid, O.paramkind})
  = do
    c <- lift $ asks parameters
    -- let (pID, pExp):_ = filter (\(a, b) -> a == paramid) c
    -- pExp' <- trExpr pExp
    let pExp = A.Param paramid
    return $ pExp

trExpr (O.AGGREF { O.aggfnoid, O._args, O.aggstar = O.PgBool star })
  = do
    aggname <- getProcData aggfnoid

    let args = unList _args
    args' <- mapM ((liftM A.expr) . trTargetEntry) args

    return $ A.AGG
            { A.aggname = aggname
            , A.args    = args'
            , A.star    = star
            }

trExpr (O.BOOLEXPR { O.boolop, O.args })
  = do
    let args' = unList args
    args'' <- mapM trExpr args'

    return $ A.BoolOp
              { A.operator = boolop
              , A.args     = args'' }

trExpr (O.NULLTEST { O.arg, O.nulltesttype })
  = do
    arg' <- trExpr arg
    return $ A.NullTest
              { A.arg = arg'
              , A.isnull = nulltesttype == 0
              }

trExpr err = error $ "Not implemented: " ++ PP.ppShow err


getProcData :: Rule Integer String
getProcData oid = do
    procRow     <- getRTableRow (pg_proc, findRow "oid" oid)
    when (null procRow) $ error "getProcData empty list"
    let proname = fromSql $ head procRow M.! "proname"
    return proname

getOperatorData :: Rule Integer String
getOperatorData oid = do
    -- Lookup the operator name in pg_operators
    procRow     <- getRTableRow (pg_operators, findRow "oid" oid)
    let oprname = fromSql $ head procRow M.! "oprname"
    return oprname

accessPgAttribute :: Rule Integer (String, [String])
accessPgAttribute relid = do
    -- Get row of relation in pg_class
    classRow <- getRTableRow (pg_class, findRow "oid" relid)
    -- Fetch relname from row
    let relname = fromSql $ head classRow M.! "relname"
    -- Fetch rows from pg_attribute with information about the columns
    attRows <- getRTableRow (pg_attribute, findRow "attrelid" relid)
    -- extract the column names from the rows
    let colnamesSch = map (\x -> fromSql $ x M.! "attname") attRows
    return (relname, colnamesSch)

trType :: Rule Integer String
trType i = do
    row <- getRTableRow (pg_type, findRow "oid" i)
    when (null row) $ error $ "type not found: " ++ (show i)
    let typecat = fromSql $ (\(x:_) -> x) row M.! "typcategory"
    let typname = fromSql $ (\(x:_) -> x) row M.! "typname"
    case typecat of
        "A" -> do
            -- Infere array subtype
            let base = fromSql $ (\(x:_) -> x) row M.! "typelem"
            row' <- trType base
            return row'
        "B" -> return typname
        -- "C" -> do --return $ A.TComposite typname
        --     let typrelid = fromSql $ (\(x:_) -> x) row M.! "typrelid"
        --     (relname, _, cols) <- accessPgAttribute typrelid
        --     return $ A.TComposite relname cols
        _ -> return typname

prodVars :: A.Plan -> [(Integer, String)]
prodVars A.Projection { A.targetList, A.leftTree } = map targetExToVar targetList
prodVars A.Selection { A.predicate, A.leftTree } = prodVars leftTree
prodVars A.Rename { A.rename, A.leftTree } = prodVars leftTree
prodVars A.Union { A.leftTree, A.rightTree } = prodVars leftTree
prodVars A.Difference { A.leftTree, A.rightTree } = prodVars leftTree
prodVars A.Join { A.leftTree, A.rightTree, A.predicate } = prodVars leftTree ++ prodVars rightTree
prodVars A.Sort { A.leftTree } = prodVars leftTree
prodVars A.DependentJoin { A.leftTree, A.rightTree, A.predicate } = prodVars leftTree ++ prodVars rightTree
prodVars A.GroupBy { A.leftTree, A.targetList } = map targetExToVar targetList
prodVars A.Map { A.leftTree, A.targetList } = map targetExToVar targetList
prodVars A.Limit { A.leftTree } = prodVars leftTree

prodVars A.Scan { A.alias, A.colnames } = zip [1..] colnames
prodVars A.Constant { A.targetList } = map targetExToVar targetList
prodVars A.D { A.leftTree } = prodVars leftTree

targetExToVar :: A.TargetExpr -> (Integer, String)
targetExToVar (A.TargetExpr { A.resname = Just s, A.resno }) = (resno, s)
