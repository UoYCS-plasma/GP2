module Cassava.Compile (compileGPProg) where

import GPSyntax
import Cassava.Instructions
import Data.List

notImplemented s = error $ "Not implemented: " ++ s

type Interface = [NodeName]


compileGPProg :: GPProgram -> Prog
compileGPProg (Program ds) = concatMap compileDecl ds


compileDecl :: Declaration -> Prog
compileDecl (MainDecl m) = compileMain m
compileDecl (ProcDecl p) = compileProc p
compileDecl (AstRuleDecl r) = compileRule r
compileDecl _ = notImplemented "compileDecl"

compileMain :: Main -> Prog
compileMain (Main cs) = PROC "Main" : concatMap compileComm cs ++ [RET]

compileProc :: Procedure -> Prog
compileProc (Procedure _ ds cs) = concatMap compileDecl ds ++ concatMap compileComm cs

compileComm :: Command -> Prog
compileComm (Block b) = compileBlock b
compileComm (IfStatement _ _ _) = notImplemented "compileComm"
compileComm (TryStatement _ _ _) = notImplemented "compileComm"

compileBlock :: Block -> Prog
compileBlock (ComSeq cs) = concatMap compileComm cs
compileBlock (LoopedComSeq cs) = block ++ [ JS (negate $ length block) ]
    where
        block = compileBlock (ComSeq cs)
compileBlock (SimpleCommand s) = compileSimple s
compileBlock (ProgramOr _ _) = notImplemented "compileBlock"


compileSimple :: SimpleCommand -> Prog
compileSimple (RuleCall rs) = map (\id -> CALL id) rs
compileSimple (LoopedRuleCall rs) = map (\id -> CALL id) rs ++ [JS (negate $ length rs)]
compileSimple (ProcedureCall p) = [CALL p]
compileSimple (LoopedProcedureCall p) = [CALL p , JS (-1)]
compileSimple Skip = notImplemented "compileSimple"
compileSimple Fail = notImplemented "compileSimple"

compileRule :: AstRule -> Prog
compileRule (AstRule id _ (lhs, rhs) cond) =
    (PROC id) : compileLhs id interface cond lhs'
    ++ (if changed then compileRhs id interface rhs' else [])
    ++ [RET]
    where
        changed = lhs' /= rhs'
        interface = computeInterface lhs rhs
        (lhs', rhs') = sortNodes lhs rhs

simplifyListEquality :: (GPList -> GPList -> Condition) -> GPList -> GPList -> Condition
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



compileCond :: Condition -> [(NodeName, Prog)]
compileCond NoCondition = []
compileCond (Eq [Indeg n]  [Val (Int i)]) = [(n, [NCI, NCL])] -- TODO: vast oversimplification!
compileCond (Eq [Outdeg n] [Val (Int i)]) = [(n, [NCO, NCL])] -- TODO: vast oversimplification!
compileCond (Not (Edge a b _)) = [(a, [EF, JS (-2)])]  -- TODO: just wrong!
compileCond (Edge a b _) = []
compileCond (Not c)   = notImplemented "compileCond Not"
compileCond (Or c d)  = notImplemented "compileCond Or"
compileCond (And c d) = notImplemented "compileCond And"
compileCond _ = notImplemented "compileCond"

compileLhs :: RuleName -> Interface -> Condition -> AstRuleGraph -> Prog
compileLhs rid nif cond (AstRuleGraph ns es) =
    concatMap (compileLhsNode rid nif es) ns
    where
        cond' = simplifyConds cond
compileLhsNode :: RuleName -> Interface -> [AstRuleEdge] -> RuleNode -> Prog
compileLhsNode rid interface es n@(RuleNode id root _) = subProc : 
    lhsNodeCodeGen root isINode (classifyEdgesForNode n es) n
    where
        subProc = PROC $ rid ++ ":lhs_" ++ id
        isINode = id `elem` interface



lhsNodeCodeGen :: Bool -> Bool -> (Deg, Deg, Deg) -> RuleNode -> Prog
lhsNodeCodeGen root inode (i, o, l) n = finst : []
    where
        finst = case (root, inode) of 
                    (True, True)   -> FRIN o i l
                    (True, False)  -> FRN  o i l
                    (False, True)  -> FIN  o i l
                    (False, False) -> FN   o i l


-- TODO: actually compile RHS
compileRhs :: RuleName -> Interface -> AstRuleGraph -> Prog
compileRhs rid _ _ = [PROC $ (rid ++ ":rhs") , COMMIT]


compileRhsDelNode :: RuleNode -> Prog
compileRhsDelNode _ = notImplemented "compileRhsDelNode"



-- Classify edges as in out or loop
data EdgeType = InEdge | OutEdge | LoopEdge | UninterestingEdge deriving Eq

classifyEdgeForId :: NodeName -> AstRuleEdge -> EdgeType
classifyEdgeForId id (AstRuleEdge _ _ i o _) =
    case (i==id, o==id) of
        (True, True) -> LoopEdge
        (True, _)    -> OutEdge
        (_, True)    -> InEdge
        _            -> UninterestingEdge

classifyEdgesForNode :: RuleNode -> [AstRuleEdge] -> (Deg, Deg, Deg)
classifyEdgesForNode (RuleNode id _ _) es = (ins, outs, loops)
    where
        ins   = length $ filter (==InEdge) cs
        outs  = length $ filter (==OutEdge) cs
        loops = length $ filter (==LoopEdge) cs
        cs = map (classifyEdgeForId id) es

-- TODO: replace null sort function with one that puts the most informative first.
sortNodes :: AstRuleGraph -> AstRuleGraph -> (AstRuleGraph, AstRuleGraph)
sortNodes lhs rhs = (lhs, rhs)

-- Compute the interface
computeInterface :: AstRuleGraph -> AstRuleGraph -> Interface
computeInterface (AstRuleGraph lns _) (AstRuleGraph rns _) = interface
    where
        lids = map extractNodeName lns
        rids = map extractNodeName rns
        interface = intersect lids rids
        extractNodeName :: RuleNode -> RuleName
        extractNodeName (RuleNode id _ _) = id

