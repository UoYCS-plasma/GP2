module RunProgram where

import ApplyRule
import Graph (emptyGraph)
import GraphIsomorphism
import GPSyntax

-- Represents a state in the program execution.
-- First component is the current graph.
-- Second component is the number of isomorphic copies of the current graph 
-- encountered during program execution.
-- Third component is the number of rules applied to reach the graph state.
data GraphState = GS (HostGraph, Int, Int)
                | Failure 

-- Output: List of (finished) host graphs, number of unfinished executions.
-- Failureed executions produce the empty graph. # failures = # empty graphs.
-- TODO: How and where to check when the max rule applications has been reached? 
--           Note that this is done per graph, not globally. How to stop execution for one graph?
-- TODO: Filter out isomorphic graphs at this stage and keep a count.

runProgram :: GPProgram -> Int -> HostGraph -> [GraphState]
runProgram (Program ds) max g = evalMain max ds (findMain ds) (GS (g, 1, 0))
        
findMain :: [Declaration] -> Main
findMain ((MainDecl m):ds) = m
findMain (_:ds) = findMain ds
findMain [] = error "No main procedure defined."

evalMain :: Int -> [Declaration] -> Main -> GraphState -> [GraphState]
evalMain max ds (Main coms) gs = evalCommandSequence max ds coms gs

evalCommandSequence :: Int -> [Declaration] -> [Command] -> GraphState -> [GraphState]
evalCommandSequence max ds [] gs = [gs]
evalCommandSequence max ds (c:cs) gs =
    case evalCommand max ds c gs of 
        [Failure] -> [Failure]
        hs -> concatMap (evalCommandSequence max ds cs) unfin ++ fin
              where (fin, unfin) = filterGSs max hs

-- Partition the list of graph states into those that have reached the 
-- bound and those that have not.
filterGSs :: Int -> [GraphState] -> ([GraphState], [GraphState])
filterGSs _ [] = ([], [])
filterGSs max (Failure:gss) = 
    (Failure:fin, unfin) where (fin, unfin) = filterGSs max gss
filterGSs max (gs:gss)  = 
    let (fin, unfin)    = filterGSs max gss
        (GS (_, _, rc)) = gs in
    if rc == max 
        then (gs:fin, unfin)         
        else (fin, gs:unfin) 


evalCommand :: Int -> [Declaration] -> Command -> GraphState -> [GraphState]
evalCommand max ds (Block b) gs = evalBlock max ds b gs 
evalCommand max ds (IfStatement cond pass fail) gs = 
    case evalBlock max ds cond gs of 
        [Failure] -> evalBlock max ds fail gs
        _        -> evalBlock max ds pass gs
evalCommand max ds (TryStatement cond pass fail) gs = 
    case evalBlock max ds cond gs of
        [Failure] -> evalBlock max ds fail gs
        hs       -> concatMap (evalBlock max ds pass) hs


evalBlock :: Int -> [Declaration] -> Block -> GraphState -> [GraphState]
evalBlock max ds (ComSeq cs) gs = evalCommandSequence max ds cs gs
evalBlock max ds (LoopedComSeq cs) gs = 
    case evalCommandSequence max ds cs gs of
        [Failure] -> [Failure]
        hs     -> concatMap (evalCommandSequence max ds cs) hs
evalBlock max ds (SimpleCommand sc) gs = evalSimpleCommand max ds sc gs
evalBlock max ds (ProgramOr b1 b2) gs = evalBlock max ds b1 gs


evalSimpleCommand :: Int -> [Declaration] -> SimpleCommand -> GraphState -> [GraphState]
evalSimpleCommand _ _ _ Failure = [Failure]
evalSimpleCommand max ds (RuleCall rs) (GS (g, ic, rc)) = 
    -- Apply all rules in the set at the same time.
    let resultGraphs = [h | r <- rs, h <- applyRule g $ ruleLookup r ds] in
    case resultGraphs of
        [] -> [Failure]
        hs -> [GS (meld h (rc+1)) | h <- getIsomorphismData (g, ic) hs]
              where meld (x, y) z = (x, y, z)
evalSimpleCommand max ds c@(LoopedRuleCall rs) (GS (g, ic, rc)) = 
    case evalSimpleCommand max ds (RuleCall rs) (GS (g, ic, rc)) of
        -- Loop terminates, return input GraphState
        [] -> [GS (g, ic, rc)]
        -- One rule call successful. If the bound has been reached, stop and return hs,
        -- otherwise continue with the loop.
        hs -> if rc+1 == max then hs else concatMap (evalSimpleCommand max ds c) hs
evalSimpleCommand max ds (ProcedureCall proc) gs = evalCommandSequence max (decls++ds) cs gs
    where Procedure id decls cs = procLookup proc ds
evalSimpleCommand max ds c@(LoopedProcedureCall proc) gs = 
    case evalSimpleCommand max ds (ProcedureCall proc) gs of
        [Failure] -> [Failure]
        hs     -> concatMap (evalSimpleCommand max ds c) hs
evalSimpleCommand max ds Skip (GS (g, ic, rc)) = [GS (g, ic, rc+1)]
evalSimpleCommand max ds Fail _ = [Failure]

procLookup :: ProcName -> [Declaration] -> Procedure
procLookup id decls = case matches of
    [] -> error $ "Reference to undefined procedure " ++ id
    _  -> head matches
    where
        matches = map (\(ProcDecl d) -> d) $ filter (p id) decls
        p :: ProcName -> Declaration -> Bool
        p id (ProcDecl (Procedure name _ _)) = id == name 
        p id _ = False

ruleLookup :: RuleName -> [Declaration] -> Rule
ruleLookup id decls = case matches of
    [] -> error $ "Reference to undefined rule " ++ id
    _  -> head matches
    where
        matches = map (\(RuleDecl d) -> d) $ filter (p id) decls
        p :: RuleName -> Declaration -> Bool
        p id (RuleDecl (Rule name _ _ _ _)) = id == name 
        p id _ = False



