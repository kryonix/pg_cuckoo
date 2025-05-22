{-# LANGUAGE NamedFieldPuns #-}

module RewriteRA where

import OperSemIO
import qualified Data.List as DL
import qualified Data.Maybe as MB
import Text.Show.Pretty as PP hiding (List, Value, Float)

import LogicalAST as A

data Context = C { boundVars :: [(String, String)] }

data StateI = StateI { phase1Dexists :: Bool }

existsD :: Rule () Bool
existsD () = do
  st <- lift $ gets phase1Dexists
  return st

phase1Dcreated :: Rule () ()
phase1Dcreated () = do
  lift $ modify (\x -> x { phase1Dexists = True })
  return ()

type Log = [String]

--------------------------------------------------------------------------------
-- Writer Monad

logRule :: Rule String ()
logRule s = do
  tell [s]
  return ()

--------------------------------------------------------------------------------

type Rule a b = a -> OperSemIO StateI Context b Log
type Rule2 a b c = a -> b -> OperSemIO StateI Context c Log

-- | Three parameter rule
type Rule3 a b c d = a -> b -> c -> OperSemIO StateI Context d Log

rewrite :: Plan -> IO (Plan, [String])
rewrite p
  = do
      ((erg, lg), state) <-
        runOperSemSTIO (startRewriting p)
                        (StateI { phase1Dexists = False})
                        (C [])
      let erg' = case erg of
                Left err -> error err
                Right f  -> f
      return (erg', lg)

elimD :: Rule Plan Plan
elimD p@D {leftTree} = do
  logRule "elimD"
  return leftTree
elimD p = mapPlan elimD p


getBinding :: Rule Plan [(Integer, Expr)]
getBinding j@(DependentJoin {binding}) = return binding
getBinding (Projection {leftTree}) = getBinding leftTree
getBinding (Selection {leftTree})  = getBinding leftTree
getBinding (Rename {leftTree})     = getBinding leftTree
getBinding (Join {leftTree, rightTree}) = do
  lb <- getBinding leftTree
  rb <- getBinding rightTree
  return $ lb ++ rb

getBinding (Union {leftTree, rightTree}) = do
  lb <- getBinding leftTree
  rb <- getBinding rightTree
  return $ lb ++ rb

getBinding (Difference {leftTree, rightTree}) = do
  lb <- getBinding leftTree
  rb <- getBinding rightTree
  return $ lb ++ rb
getBinding (Sort {leftTree}) = getBinding leftTree
getBinding (GroupBy {leftTree}) = getBinding leftTree
getBinding (Map {leftTree})     = getBinding leftTree
getBinding (Limit {leftTree})   = getBinding leftTree

getBinding _ = return []

startRewriting :: Rule Plan Plan
startRewriting p
  = do
    parameterBindings <- getBinding p
    -- p <- predicatePullUp p
    p <- rwPlan p
    p <- replaceParam parameterBindings p
    -- p <- predicatePushDown p
    -- p <- elimD p
    return p

--------------------------------------------------------------------------------
-- predicate push down rules

predicatePushDown :: Rule Plan Plan
{-
(A ⨝{p₂} B) --> (A ⨝{p₂} B)'
------------------------------------
σ{p₁} (A ⨝{p₂} B) --> (A ⨝{p₁,p₂} B)'
-}
predicatePushDown p@Selection{ predicate
                             , leftTree = j@Join { predicate = p1 } }
  = do
    logRule $ "Predicate push down merge Join and Selection"
    j' <- predicatePushDown j
    predicatePushDown $ j' { predicate = p1 ++ predicate }

{-
A --> A
-}
predicatePushDown e = mapPlan predicatePushDown e

-- / predicate push down rules
--------------------------------------------------------------------------------
-- predicate pull up rules

mkSelection :: Plan -> [Expr] -> Plan
mkSelection p [] = p
mkSelection p xs = Selection {leftTree = p, predicate = xs}

predicatePullUp :: Rule Plan Plan
predicatePullUp j@Selection { leftTree = l, predicate=xs }
  = do
    l' <- predicatePullUp l
    logRule $ "Predicate pull up selection split dep"
    let xsDep   = filter isDependent xs
    let xsUnDep = filter (not . isDependent) xs
    return $ mkSelection (mkSelection l' xsUnDep) xsDep
-- predicatePullUp j@Selection { leftTree = l, predicate=[] } = return l
-- predicatePullUp j@Selection { leftTree = l, predicate=x:xs }
--   = do
--     logRule "Predicate pull up split selection"
--     l' <- predicatePullUp (Selection { leftTree = l, predicate = xs })
--     return $ Selection {leftTree = l', predicate = [x] }

predicatePullUp j@DependentJoin { leftTree =
                                  l@Selection { leftTree = lt
                                              , predicate=p }
                                , predicate }
  = do
    logRule "Predicate pull up merge DepJoin and Selection L"
    return $ j {leftTree = lt, predicate = p++predicate}

    -- predicatePullUp $ j {leftTree = lt, predicate = p++predicate}

predicatePullUp j@DependentJoin { rightTree = 
                                  r@Selection { leftTree = lt
                                              , predicate=p }
                                , predicate }
  = do
    logRule "Predicate pull up merge DepJoin and Selection R"
    return $ j {rightTree = lt, predicate = p++predicate}
  
predicatePullUp j@Join { leftTree =
                          l@Selection { leftTree = lt
                                      , predicate=p }
                        , predicate
                        , rightTree
                        }
  = do
    rightTree' <- predicatePullUp rightTree
    lt' <- predicatePullUp lt
    logRule "Predicate pull up merge Join and Selection L"
    return $ j {leftTree = lt', predicate = p++predicate, rightTree = rightTree'}
  
predicatePullUp j@Join { rightTree =
                          r@Selection { leftTree = lt
                                      , predicate=p }
                        , predicate
                        , leftTree
                        }
  = do
    leftTree' <- predicatePullUp leftTree
    lt' <- predicatePullUp lt
    logRule "Predicate pull up merge Join and Selection R"
    return $ j {rightTree = lt', predicate = p++predicate, leftTree = leftTree'}

{-
A --> A'            B --> B'
---------------------------
A ⧑{p} B --> σ{p}(A' ⧑ B')
-}
predicatePullUp j@DependentJoin { leftTree = l
                                , rightTree = r
                                , predicate = xs }
  = do
    l' <- predicatePullUp l
    r' <- predicatePullUp r
    let newDPj = j { predicate = [], leftTree = l', rightTree = r' }
    let newRoot = mkSelection newDPj xs
    when (j /= newRoot) $ logRule "Predicate pull up DependentJoin"
    return newRoot

{-
A --> A'            B --> B'
---------------------------
A ⨝{p} B --> σ{p}(A' ⨝ B')
-}
predicatePullUp j@Join { leftTree = l, rightTree = r, predicate = xs}
  = do
    l' <- predicatePullUp l
    r' <- predicatePullUp r
    let newDPj = j { predicate = [], leftTree = l', rightTree = r' }
    let newRoot = mkSelection newDPj xs
    when (j /= newRoot) $ logRule "Predicate pull up RegularJoin"
    return newRoot

predicatePullUp e = mapPlan predicatePullUp e

-- / predicate pull up rules
--------------------------------------------------------------------------------
-- unnesting rules

rwPlan :: Rule Plan Plan
{-
dependent join push down selection
    (D ⧑ T2) --> (D ⧑ T2)'
-----------------------------
D ⧑ σ{p}(T2) ≡ σ{p}(D ⧑ T2)'
-}
rwPlan j@DependentJoin { leftTree = D {}, rightTree = sel@(Selection {}), predicate, joinType }
  = do
    logRule "Rewrite push down dependent join selection"
    let newChild = j { rightTree = leftTree sel }
    newChild' <- rwPlan newChild
    let newRoot = sel { leftTree = newChild' }
    return newRoot

{- dependent join push down group-by
                D ⧑ T --> (D ⧑ T)'
---------------------------------------------
D ⧑ (Γ{A;a:f}(T)) --> Γ{A ∪ A(D);a:f}(D ⧑ T)'
-}
rwPlan j@DependentJoin { leftTree = l@(D {})
                       , rightTree = gb@(GroupBy { leftTree = gbL
                                                 , groupCols
                                                 , targetList = targetListGB
                                                 })
                       }
  = do
    logRule "Rewrite push down dependent join group by"
    -- Calculate A(D)
    let ad = prodVars l
    
    liftIO $ putStrLn $ show ad

    -- Construct D ⧑ T
    let newChild = j { rightTree = gbL }
    -- recurse unnesting
    newChild' <- rwPlan newChild

    let ad' = zip (prodVars newChild') [1..]
    let adPos = MB.catMaybes $ map (flip lookup ad') ad

    let groupCols' = groupCols ++ ad --adPos

    let maxRN = maximum (map resno targetListGB)

    let djSch = [ TargetExpr { expr = A.VAR "l" x, resname = Just x, resno = rn }
                | (x, rn) <- zip groupCols' [(maxRN+1)..] ]

    let newRoot = GroupBy
                  { leftTree = newChild'
                  , groupCols = groupCols'
                  , targetList = targetListGB ++ djSch }

    return newRoot

{- dependent join push down projection D ⧑ (Π{A} (T)) ≡ Π{A ∪ A(D)} (D ⧑ T)
-}

rwPlan j@(DependentJoin { leftTree = l@(D {})
                        , rightTree = pj@(Projection {targetList, leftTree = pjL})
                        })
  = do
    logRule "Rewrite push down dependent join projection"

    -- Calculate A(D)
    let ad = prodVars l

    -- Construct D ⧑ T
    let newChild = j { rightTree = pjL }
    -- recurse unnesting
    newChild' <- rwPlan newChild

    let ad' = zip (prodVars newChild') [1..]
    let adPos = MB.catMaybes $ map (flip lookup ad') ad

    let pjCols = ad
    let maxRN = maximum (map resno targetList)

    let djSch = [ TargetExpr { expr = A.VAR "l" x, resname = Just x, resno = rn}
                | (x, rn) <- zip pjCols [(maxRN+1)..] ]

    let newRoot = Projection
                  { leftTree = newChild'
                  , targetList = targetList ++ djSch }

    return newRoot

{- dependent join push down join
  
  D ⧑ (T₁ ⨝ₚ T₂) ≡ (D ⧑ T₁) ⨝ₚ iff T₂ is not correlated
  D ⧑ (T₁ ⨝ₚ T₂) ≡ T₁ ⨝ₚ (D ⧑ T₂) iff T₁ is not correlated
  D ⧑ (T₁ ⨝ₚ T₂) ≡ (D ⧑ T₁) ⨝ₚ ∧ natural join D (D ⧑ T₂) iff T₁ otherwise
-}

rwPlan j@(DependentJoin { leftTree = l@(D {})
                        , rightTree =
                          sj@(Join { leftTree = lt
                                   , rightTree = rt
                                   , predicate = p
                                   , joinType
                                   })
                        })
  = do
    logRule "Rewrite push down dependent join regular join"
    -- Check left and right side for correlation
    corrL <- freeVars lt
    corrR <- freeVars rt

    -- This might be insufficient. Some join methods require specific push down
    -- operations of the dependent join. (e.g. always push to both sides for FULL OUTER JOIN)
    let force = case joinType of
                  INNER -> False
                  _     -> True

    let mkChild c p = if null c && not force then p else j { rightTree = p }

    let lt' = mkChild corrL lt
    let rt' = mkChild corrR rt

    -- recurse unnesting
    lt' <- rwPlan lt'
    rt' <- rwPlan rt'

    let p' = p ++ [ Operator "=" [ VAR "l" c, VAR "r" c ] | c <- prodVars l ]

    -- Create new root. Do not change the predicate if left or right side is
    -- uncorrelated. p' holds the natural join predicate.
    let newRoot = if null corrL || null corrR
                  then sj { leftTree = lt', rightTree = rt' }
                  else sj { leftTree = lt', rightTree = rt', predicate = p' }

    return newRoot


-- EXPERIMENTAL
rwPlan j@(DependentJoin { leftTree = l@(D {leftTree = domLeft@(Projection {targetList = tl1})})
                        , rightTree =
                          c@(Constant {targetList})
                        , binding
                        , joinType
                        })
  = do
    logRule $ "[EXPERIMENTAL] Rewrite push down dependent join constant"

    -- let newRoot = Projection { leftTree = l, targetList = targetList }

    -- return newRoot
    let ad = [(y) | (_, VAR {tableName = x, colName = y}) <- binding ] -- prodVars l
    let ad' = zip (prodVars l) [1..]
    let adPos = MB.catMaybes $ map (flip lookup ad') ad

    let pjCols = ad
    logRule $ show pjCols
    let maxRN = maximum (map resno targetList)

    let djSch = [ TargetExpr { expr = A.VAR "l" x, resname = Just x, resno = rn}
                | (x, rn) <- zip pjCols [(maxRN+1)..] ]

    let newRoot = Join { joinType = joinType
                    , leftTree = l
                    , predicate = [ Operator
                                    { operator = "="
                                    , args = [ VAR { tableName = "l", colName = v }
                                             , VAR { tableName = "r", colName = v }
                                             ]
                                    }
                                  | v <- ad
                                  ]
                    , rightTree =
                      Projection { targetList = targetList++djSch, leftTree = l } 
                    }

    let newTL = [ TargetExpr e r n | (TargetExpr e r _, n) <- zip (tl1 ++ targetList) [1..] ]
    let test = l { leftTree = domLeft { targetList = newTL } }

    -- let newRoot = D $ Projection { targetList = targetList ++ djSch, leftTree = leftTree l }
    return test
    --rwPlan newRoot

rwPlan j@(DependentJoin
          { leftTree = l
          , rightTree = 
            r@(Limit { leftTree=leftTree@(Projection { targetList, leftTree = lt }) })
          , binding })
  = do
    logRule $ "[EXPERIMENTAL] Rewrite push down dependent join LIMIT"
    let ad = [(y) | (_, VAR {tableName = x, colName = y}) <- binding ] -- prodVars l
    let prodVarsL = ad

    let maxRN = maximum (map resno targetList)

    let newRoot = Join { joinType = LEFT
                    , leftTree = l
                    , predicate = [ Operator
                                    { operator = "="
                                    , args = [ VAR { tableName = "l", colName = v }
                                             , VAR { tableName = "r", colName = v }
                                             ]
                                    }
                                  | v <- ad
                                  ]
                    , rightTree = D
                      leftTree { targetList = targetList ++
                                    [ A.TargetExpr { expr = A.VAR "l" x, resname = Just x, resno=rn }
                                    | (x, rn) <- zip prodVarsL [(maxRN+1)..]]
                                 , leftTree = lt }
                    }

    -- let newRoot = D $ Projection { targetList = targetList ++ djSch, leftTree = leftTree l }

    rwPlan newRoot

    -- let newRoot = j { rightTree = leftTree }
    -- rwPlan newRoot

{-
D ≡ Π F(T₂) ∩ A(T₁) (T₁)
----------------------------------------------
T₁ ⧑{p} T₂ ≡ T₁ ⨝{p ∧ T₁ = A(D) D } (D ⧑ T₂)
-}
rwPlan j@(DependentJoin { leftTree, rightTree, predicate, joinType, binding })
  = do
    -- Calculate F(T₂)
    freeVarsR <- freeVars rightTree
    -- Calculate A(T₁)
    let prodVarsL = prodVars leftTree
    -- Calculate F(T₂) ∩ A(T₁)
    let schemaD = (map (\(x,y) -> x ++ "." ++ y) freeVarsR) `DL.intersect` prodVarsL
    -- let schemaD = (map (\(x,y) -> y) freeVarsR) `DL.intersect` prodVarsL

    -- Use bindings of DependentJoin
    let bindings = [i | (i, _) <- binding]

    let params = [(read n) :: Integer | (_, n) <- freeVarsR ]

    let schemaD1 = bindings `DL.intersect` params

    let schemaD = [ "d." ++ c | (i, VAR _ c) <- binding, i `elem` schemaD1]

    logRule $ "Rewrite depJoin\nF(T₂) =\n" ++ PP.ppShow freeVarsR 
              ++ "\nA(T₁) =\n" ++ PP.ppShow prodVarsL
    if null schemaD
    then do
      -- Case 1: Transform dependent join into a regular join,
      -- if the right hand side no longer depends on the left hand side
      -- D ⧑ T ≡ D ⨝ T if F(T) ∩ A(D) = ∅
      logRule "Rewrite transform depJoin to regJoin"
      let j = Join
              { leftTree  = leftTree
              , rightTree = rightTree
              , predicate = predicate
              , joinType  = joinType }
      let j' = j
      -- j' <- replaceParam binding j
      return j'

    else do
      logRule $ "Rewrite create D: " ++ show schemaD
      stExistsD <- existsD ()
      when stExistsD $ error "Should not recurse again into this case"
      -- Case 2: Create D as Π F(T₂) ∩ A(T₁) (T₁)
      let planD = D Projection
                  { targetList = 
                    [ TargetExpr
                      { expr = VAR "l" x
                      , resname = Just x
                      , resno   = rn }
                    | (x', rn) <- zip schemaD [1..]
                    , let x = if '.' `elem` x'
                                then tail $ dropWhile (/= '.') x'
                                else x'
                    ]
                  , leftTree = leftTree }

      let p' = [ Operator { operator = "="
                          , args =
                            [ VAR "l" x
                            , VAR "r" x ]
                          }
                | x' <- schemaD
                , let x = if '.' `elem` x'
                              then tail $ dropWhile (/= '.') x'
                              else x' ]
      let depJoin' = j { leftTree = planD, predicate = [] }
      -- depJoin' <- replaceParam binding depJoin'
      phase1Dcreated ()

      -- Recurse rwPlan in new dependent join
      depJoin'' <- rwPlan depJoin'

      let j' = Join
                { leftTree = leftTree
                , rightTree  = depJoin''
                , predicate = predicate ++ p'
                , joinType = joinType }
      -- j' <- replaceParam binding j'

      liftIO $ putStrLn $ show freeVarsR ++ " " ++ show prodVarsL ++ " " ++ show schemaD
      return j'

rwPlan p = mapPlan rwPlan p

-- / unnesting rules
--------------------------------------------------------------------------------

replaceParam :: Rule2 [(Integer, Expr)] Plan Plan
replaceParam context p@(Projection {targetList, leftTree})
  = do
    tl' <- mapM (replaceParamT context) targetList
    leftTree' <- replaceParam context leftTree
    return p { targetList = tl', leftTree = leftTree' }

replaceParam context p@(Selection {predicate, leftTree})
  = do
    predicate' <- mapM (replaceParamE context) predicate
    leftTree' <- replaceParam context leftTree
    return p { predicate = predicate', leftTree = leftTree' }

replaceParam context p@(Constant { targetList })
  = do
    tl' <- mapM (replaceParamT context) targetList
    return p { targetList = tl' }

replaceParam context p@(Join { leftTree, rightTree, predicate })
  = do
    predicate' <- mapM (replaceParamE context) predicate
    leftTree' <- replaceParam context leftTree
    rightTree' <- replaceParam context rightTree
    return p { predicate = predicate', leftTree = leftTree', rightTree = rightTree' }

replaceParam context p@(DependentJoin { leftTree, rightTree, predicate })
  = do
    predicate' <- mapM (replaceParamE context) predicate
    leftTree' <- replaceParam context leftTree
    rightTree' <- replaceParam context rightTree
    return p { predicate = predicate', leftTree = leftTree', rightTree = rightTree' }

replaceParam context p@(GroupBy {targetList, leftTree})
  = do
    tl' <- mapM (replaceParamT context) targetList
    leftTree' <- replaceParam context leftTree
    return p { targetList = tl', leftTree = leftTree' }

replaceParam context p@(Map {targetList, leftTree})
  = do
    tl' <- mapM (replaceParamT context) targetList
    leftTree' <- replaceParam context leftTree
    return p { targetList = tl', leftTree = leftTree' }

replaceParam context p@(Limit {leftTree, limitCount, limitOffset})
  = do
    leftTree' <- replaceParam context leftTree
    limitCount' <- mapM (replaceParamE context) limitCount
    limitOffset' <- mapM (replaceParamE context) limitOffset
    return p { leftTree = leftTree', limitCount = limitCount', limitOffset = limitOffset' }

replaceParam context p = mapPlan (replaceParam context) p

replaceParamT :: Rule2 [(Integer, Expr)] TargetExpr TargetExpr
replaceParamT context e@(TargetExpr { expr })
  = do
    e' <- replaceParamE context expr
    return e { expr = e' }


replaceParamE :: Rule2 [(Integer, Expr)] Expr Expr
replaceParamE context Param { paramid }
  = do
    case paramid `lookup` context of
      Nothing -> return Param { paramid }
      Just (VAR t c)  -> return (VAR "d" c)
      Just f          -> mapExpr (replaceParamE context) f

replaceParamE context e = mapExpr (replaceParamE context) e

fv :: Plan -> IO [(String, String)]
fv p = do
        ((erg, lg), state) <- runOperSemSTIO (freeVars p) (StateI False) (C [])
        let erg' = case erg of
                  Left err -> error err
                  Right f  -> f
        return erg'

freeVars :: Rule Plan [(String, String)]
freeVars Projection { targetList, leftTree } 
  = do
    l <- freeVars leftTree
    tl <- mapM freeVarsExpr (map expr targetList)
    return $ l ++ concat tl

freeVars Selection { predicate, leftTree }
  = do
    l <- freeVars leftTree
    p' <- mapM freeVarsExpr predicate
    return $ l ++ concat p'
freeVars Rename { rename, leftTree } = freeVars leftTree
freeVars Union { leftTree, rightTree }
  = do
    l <- freeVars leftTree
    r <- freeVars rightTree
    return $ l ++ r
freeVars Difference { leftTree, rightTree } 
  = do
    l <- freeVars leftTree
    r <- freeVars rightTree
    return $ l ++ r
freeVars Join { leftTree, rightTree, predicate }
  = do
    l <- freeVars leftTree
    r <- freeVars rightTree
    let schema = (zip (repeat "l") $ prodVars leftTree) ++ (zip (repeat "r") $ prodVars rightTree)
    p' <- local (const (C schema)) $ mapM freeVarsExpr predicate
    return $ l ++ r ++ concat p'
freeVars Sort { leftTree } = freeVars leftTree
freeVars DependentJoin { leftTree, rightTree, predicate }
  = do
    l <- freeVars leftTree
    r <- freeVars rightTree
    let schema = (zip (repeat "l") $ prodVars leftTree) ++ (zip (repeat "r") $ prodVars rightTree)
    p' <- local (const (C schema)) $ mapM freeVarsExpr predicate
    return $ l ++ r ++ concat p'
freeVars GroupBy { targetList, leftTree }
  = do
    l <- freeVars leftTree
    tl <- mapM freeVarsExpr (map expr targetList)
    return $ l ++ concat tl

freeVars Map { targetList, leftTree }
  = do
    l <- freeVars leftTree
    tl <- mapM freeVarsExpr (map expr targetList)
    return $ l ++ concat tl

freeVars Limit { leftTree } = freeVars leftTree
freeVars D {leftTree} = freeVars leftTree
freeVars Constant { targetList }
  = do
    l <- mapM freeVarsExpr (map expr targetList)
    return $ concat l
freeVars _ = return []

freeVarsExpr :: Rule Expr [(String, String)]
freeVarsExpr (Const {}) = return []
freeVarsExpr (Operator { args })
  = do
    args' <- mapM freeVarsExpr args
    return $ concat args'

freeVarsExpr (Param { paramid })
  = do
    return [("d", show paramid)]

-- freeVarsExpr (VAR { tableName, colName })
--   = do
--     c <- lift $ asks boundVars
--     case (tableName, colName) `elem` c of
--       False -> return [(tableName, colName)]
--       True  -> return []

-- freeVarsExpr (VARSUB { colName })
--   = do
--     c <- lift $ asks boundVars
--     case ("", colName) `elem` c of
--       False -> return [("", colName)]
--       True  -> return []

freeVarsExpr (AGG { args })
  = do
    args' <- mapM freeVarsExpr args
    return $ concat args'

freeVarsExpr (Function { args })
  = do
    args' <- mapM freeVarsExpr args
    return $ concat args'

freeVarsExpr (BoolOp { args })
  = do
    args' <- mapM freeVarsExpr args
    return $ concat args'

freeVarsExpr (DEP { arg })
  = freeVarsExpr arg

freeVarsExpr (NullTest { arg })
  = freeVarsExpr arg

freeVarsExpr _ = return []

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
prodVars Scan { alias, colnames } = colnames ++ map (\x -> alias ++ "." ++ x) colnames
-- prodVars Scan { alias, colnames } = colnames ++ map (\x -> "l." ++ x) colnames ++ map (\x -> alias ++ "." ++ x) colnames
prodVars Constant { targetList } = map targetExToVar targetList
prodVars D { leftTree } = prodVars leftTree

targetExToVar :: TargetExpr -> String
targetExToVar (TargetExpr { resname = Just s }) = s

isDependent :: Expr -> Bool
isDependent (Const {})          = False
isDependent (Operator { args }) = any isDependent args
isDependent (Function { args }) = any isDependent args
isDependent (DEP {})            = True
isDependent (VAR {})            = False
isDependent (VARSUB {})         = False
isDependent (AGG { args })      = any isDependent args
isDependent (BoolOp { args })   = any isDependent args
isDependent (NullTest { arg })  = isDependent arg
isDependent (Param {})          = True
