module OilrMachine.Compile (compileGPProg, RegisterMap) where

import GPSyntax
import OilrMachine.Instructions
import OilrMachine.Analysis
import Data.List

import Debug.Trace

notImplemented s = error $ "Not implemented: " ++ s

type RegisterMap = [(NodeName, Int)]


compileGPProg :: GPProgram -> Prog
compileGPProg (Program ds) = concat $ reverse $ map compileDecl ds


compileDecl :: Declaration -> Prog
compileDecl (MainDecl m) = compileMain m
compileDecl (ProcDecl p) = compileProc p
compileDecl (AstRuleDecl r) = compileRule r
compileDecl _ = notImplemented "compileDecl"

compileMain :: Main -> Prog
compileMain (Main cs) = PROC "Main" : concatMap compileComm cs ++ [RET]

compileProc :: Procedure -> Prog
compileProc (Procedure id ds cs) = PROC id : concatMap compileDecl ds ++ concatMap compileComm cs ++ [RET]

compileComm :: Command -> Prog
compileComm (Block b) = compileBlock b
compileComm (IfStatement _ _ _) = notImplemented "compileComm"
compileComm (TryStatement _ _ _) = notImplemented "compileComm"

compileBlock :: Block -> Prog
compileBlock (ComSeq cs) = concatMap compileComm cs
compileBlock (LoopedComSeq cs) = notImplemented "LoopedComSeq" -- block ++ [ JS (negate $ length block) ]
    where
        block = compileBlock (ComSeq cs)
compileBlock (SimpleCommand s) = compileSimple s
compileBlock (ProgramOr _ _) = notImplemented "compileBlock"


compileSimple :: SimpleCommand -> Prog
compileSimple (RuleCall rs) = concatMap (\id -> [CALL id, ZTRF]) rs
compileSimple (LoopedRuleCall rs) = map (\id -> LOOP id) rs
compileSimple (ProcedureCall p) = [CALL p , ZTRF]
compileSimple (LoopedProcedureCall p) = [LOOP p]
compileSimple Skip = notImplemented "compileSimple"
compileSimple Fail = notImplemented "compileSimple"

compileRule :: AstRule -> Prog
compileRule rule@(AstRule id _ (lhs, rhs) cond) =
    case changed of
        True  -> compiledLhs ++ compiledRhs ++ [RET]
        false -> compiledLhs ++ [RET]
    
    where
        -- TODO: refactor this to eliminate all these primes and make the TRAV ordering less
        -- dependent upon the type
        compiledLhs = (PROC id) : nodeTravs ++ edgeTravs ++ conditions ++ [GO, ZTRF]
        compiledRhs = if modifies rc then DELE : DELN : updatedNodes ++ createEdges rmap''' rhs' else []
        (nodeTravs, rmap)    = travsForLhsNodes rc lhs'
        (edgeTravs, rmap')   = travsForLhsEdges rc rmap lhs'
        (conditions, rmap'') = compileCond rmap' lhs' cond
        rc  = characteriseRule rule
        (updatedNodes, rmap''') = updateNodes rc rmap'' rhs'
        changed    = lhs' /= rhs'
        (lhs', rhs') = sortNodes lhs rhs


travsForLhsNodes :: RuleCharacterisation -> AstRuleGraph -> (Prog, RegisterMap)
travsForLhsNodes rc (AstRuleGraph ns es) = (map (travForLhsNode rc es) ns, rmap)
    where
        rmap = zip (map (\(RuleNode id _ _) -> id) ns) [0..]


travForLhsNode :: RuleCharacterisation -> [AstRuleEdge] -> RuleNode -> Instr
travForLhsNode rc es (RuleNode id root _) =
    case (root, id `nodeInterfaceElem` rc) of
        (True, True)   -> TRIN o i l
        (True, False)  -> TRN  o i l
        (False, True)  -> TIN  o i l
        (False, False) -> TN   o i l
    where
        (o, i, l) = classifyEdgesForNode id es

travsForLhsEdges :: RuleCharacterisation -> RegisterMap -> AstRuleGraph -> (Prog, RegisterMap)
travsForLhsEdges rc rmap (AstRuleGraph ns es) = (map (travForLhsEdge rc rmap) es, rmap')
    where
        rmap' = rmap ++ zip (map (\(AstRuleEdge id _ _ _ _) -> id) es) [length rmap..]

travForLhsEdge :: RuleCharacterisation -> RegisterMap -> AstRuleEdge -> Instr
travForLhsEdge rc rmap e@(AstRuleEdge id bidi src tgt _) =
    case (bidi, id `edgeInterfaceElem` rc, lookup src rmap, lookup tgt rmap) of
        (False, False, Just n, Just m)  -> TE n m
        (True,  False, Just n, Just m)  -> TBE n m
        (False, True,  Just n, Just m)  -> CE n m
        (True,  True,  Just n, Just m)  -> CBE n m
        _ -> error $ "Reference to a node without a traverser in edge " ++ id


-- TODO: Incorrect behaviour. ROOT and TOOR should only be issued if root status changes
-- between lhs and rhs
updateNodes :: RuleCharacterisation -> RegisterMap -> AstRuleGraph -> (Prog, RegisterMap)
updateNodes rc rmap (AstRuleGraph ns es) = (updated ++ created, rmap')
    where
        (updateMe, createMe) = partition (\(RuleNode id _ _) -> id `nodeInterfaceElem` rc) ns
        updated = concatMap updateNode updateMe
        (created, rmap') = createNodes rmap [] createMe
        updateNode (RuleNode id root _) = [] {- case (lookup id rmap, root) of
            (Just n, True)  -> [ROOT n]
            (Just n, False) -> [TOOR n]
            _               -> error "Node not found!" -}
        nids  = map (\(RuleNode id _ _) -> id) ns

createNodes :: RegisterMap -> Prog -> [RuleNode] -> (Prog, RegisterMap)
createNodes rmap prog [] = trace (show rmap) (prog, rmap)
createNodes rmap prog (RuleNode id root _:ns) = createNodes rmap' prog' ns
    where
        prog' = prog ++ (if root then [NEWN, ROOT (length rmap)] else [NEWN])
        rmap' = (id, length rmap):rmap


createEdges :: RegisterMap -> AstRuleGraph -> Prog
createEdges rmap (AstRuleGraph ns es) = map mkEdge es
    where
        mkEdge (AstRuleEdge id _ src tgt _) =
            case (lookup src rmap, lookup tgt rmap) of
                    (Just n, Just m) -> NEWE n m
                    _                -> error $ "Unknown node in " ++ id

{-simplifyListEquality :: (GPList -> GPList -> Condition) -> GPList -> GPList -> Condition
simplifyListEquality cmp [a] [v] = cmp [a] [v]
simplifyListEquality cmp as vs = foldr1 (\c d -> And c d) $ map (\(a, v) -> cmp [a] [v]) $ zip as vs

-- Only equality and inequality accept GPLists as arguments
simplifyConds :: Condition -> Condition
simplifyConds (Eq as vs)  = simplifyListEquality Eq as vs
simplifyConds (NEq as vs) = simplifyListEquality NEq as vs
simplifyConds (And c d)  = And (simplifyConds c) (simplifyConds d)
simplifyConds (Or c d)   = Or (simplifyConds c) (simplifyConds d)
simplifyConds (Not c)    = Not (simplifyConds c)
simplifyConds c          = c

-}


-- TODO: fixing indeg/outdeg to non-zero or non-constant is not supported.

compileCond :: RegisterMap -> AstRuleGraph -> Condition -> (Prog, RegisterMap)
compileCond rmap lhs NoCondition = ([], rmap)
compileCond rmap lhs (Eq [Indeg n]  [Val (Int i)]) =
    if i == 0 then
        ([FIXI reg, FIXL reg], rmap)
    else 
        notImplemented "Non-zero indegree."
    where
        reg = case lookup n rmap of
            Just x -> x
            _      -> error $ "Node specified in condition not found"
compileCond rmap lhs (Eq [Outdeg n] [Val (Int i)]) =
    if i == 0 then
        ([FIXO reg, FIXL reg], rmap)
    else 
        notImplemented "Non-zero indegree."
    where
        reg = case lookup n rmap of
            Just x -> x
            _      -> error $ "Node specified in condition not found"
compileCond rmap lhs (Not (Edge a b _)) =
    case (lookup a rmap, lookup b rmap) of
        (Just n, Just m) -> ([XE n m], ("cond", length rmap):rmap)
        _                -> error "Anti-traverser creation failed!"
compileCond rmap lhs (And x y) = (progl ++ progr, rmap'')
    where
        (progl, rmap') = compileCond rmap lhs x
        (progr, rmap'') = compileCond rmap lhs y
compileCond rmap lhs c = notImplemented $ "compileCond: " ++ show c



-- Classify edges as in out or loop
data EdgeType = InEdge | OutEdge | LoopEdge | UninterestingEdge deriving Eq

classifyEdgeForId :: NodeName -> AstRuleEdge -> EdgeType
classifyEdgeForId id (AstRuleEdge _ _ i o _) =
    case (i==id, o==id) of
        (True, True) -> LoopEdge
        (True, _)    -> OutEdge
        (_, True)    -> InEdge
        _            -> UninterestingEdge

classifyEdgesForNode :: NodeName -> [AstRuleEdge] -> (Deg, Deg, Deg)
classifyEdgesForNode id es = (outs, ins, loops)
    where
        ins   = length $ filter (==InEdge) cs
        outs  = length $ filter (==OutEdge) cs
        loops = length $ filter (==LoopEdge) cs
        cs = map (classifyEdgeForId id) es

-- TODO: replace null sort function with one that puts the most informative first.
sortNodes :: AstRuleGraph -> AstRuleGraph -> (AstRuleGraph, AstRuleGraph)
sortNodes lhs rhs = (lhs, rhs)
{-sortNodes (AstRuleGraph ns es) rhs = (lhs', rhs)
    where
        lhs' = (sortBy () lhs
        sorter :: 
        -}
 


