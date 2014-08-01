module RunProgram where

import ApplyRule
import Graph (emptyGraph)
import GraphIsomorphism
import GPSyntax

-- GS represents the state of a graph during the program execution.
-- First component is the working graph.
-- Second component is the number of isomorphic copies of the working graph 
-- generated during program execution.
-- Third component is the number of rules applied to reach the graph state.	
data GraphState = GS HostGraph Int
                | Failure 
                | Unfinished
    deriving Show

-- A host graph and its isomorphism count.				
type GraphData = (HostGraph, Int) 
 
-- The output of the GP 2 interpreter is a list of output graphs, each with an 
-- isomorphism count; a failure count; and an unfinished execution count with
-- respect to the bound on rule applications. 
type Result = ([GraphData], Int, Int)

runProgram :: GPProgram -> Int -> HostGraph -> Result
runProgram (Program ds) max g = isoFilter $ processData $ evalMain max ds (findMain ds) g
    where isoFilter :: ([HostGraph], Int, Int) -> Result
          isoFilter (gs, fc, uc) = (isomorphismCount gs, fc, uc)


processData :: [GraphState] -> ([HostGraph], Int, Int)
processData = foldr addGraphState ([], 0, 0)
    where addGraphState :: GraphState -> ([HostGraph], Int, Int) -> ([HostGraph], Int, Int) 
          addGraphState (GS g rc) (gs, fc, uc) = (g:gs, fc, uc)
          addGraphState (Unfinished) (gs, fc, uc) = (gs, fc, uc+1)
          addGraphState (Failure) (gs, fc, uc) = (gs, fc+1, uc)
        
findMain :: [Declaration] -> Main
findMain ((MainDecl m):ds) = m
findMain (_:ds) = findMain ds
findMain [] = error "No main procedure defined."

evalMain :: Int -> [Declaration] -> Main -> HostGraph -> [GraphState]
evalMain max ds (Main coms) g = evalCommandSequence max ds coms (GS g 0)

evalCommandSequence :: Int -> [Declaration] -> [Command] -> GraphState -> [GraphState]
evalCommandSequence _ _ _ Failure = [Failure]
evalCommandSequence _ _ _ Unfinished = [Unfinished]
evalCommandSequence max ds [] gs = [gs]
evalCommandSequence max ds (c:cs) gs =
    case evalCommand max ds c gs of 
        [Unfinished] -> [Unfinished]
        [Failure] -> [Failure]
        hs -> concatMap (evalCommandSequence max ds cs) hs

evalCommand :: Int -> [Declaration] -> Command -> GraphState -> [GraphState]
evalCommand _ _ _ Failure = [Failure]
evalCommand _ _ _ Unfinished = [Unfinished]
evalCommand max ds (Block b) gs = evalBlock max ds b gs 
evalCommand max ds (IfStatement cond pass fail) gs = 
    case evalBlock max ds cond gs of 
        [Unfinished] -> [Unfinished]
        [Failure] -> evalBlock max ds fail gs
        _        -> evalBlock max ds pass gs
evalCommand max ds (TryStatement cond pass fail) gs = 
    case evalBlock max ds cond gs of
        [Unfinished] -> [Unfinished]
        [Failure] -> evalBlock max ds fail gs
        hs       -> concatMap (evalBlock max ds pass) hs


evalBlock :: Int -> [Declaration] -> Block -> GraphState -> [GraphState]
evalBlock _ _ _ Failure = [Failure]
evalBlock _ _ _ Unfinished = [Unfinished]
evalBlock max ds (ComSeq cs) gs = evalCommandSequence max ds cs gs
evalBlock max ds ls@(LoopedComSeq cs) gs = 
    case evalCommandSequence max ds cs gs of
        [Unfinished] -> [Unfinished]
        -- Loop terminates, return input GraphState
        [Failure] -> [gs]
        hs     -> concatMap (evalBlock max ds ls) hs
evalBlock max ds (SimpleCommand sc) gs = evalSimpleCommand max ds sc gs
evalBlock max ds (ProgramOr b1 b2) gs = evalBlock max ds b1 gs  ++ evalBlock max ds b2 gs


evalSimpleCommand :: Int -> [Declaration] -> SimpleCommand -> GraphState -> [GraphState]
evalSimpleCommand _ _ _ Failure = [Failure]
evalSimpleCommand _ _ _ Unfinished = [Unfinished]
evalSimpleCommand max ds (RuleCall rs) (GS g rc) = 
    if rc == max 
        then [Unfinished]
        -- Apply all rules in the set at the same time.
        else let resultGraphs = [h | r <- rs, h <- applyRule g $ ruleLookup r ds] in
            case resultGraphs of
                [] -> [Failure]
                hs -> [GS h (rc+1) | h <- hs]
                {- Isomorphism filtering performed after each rule application. 
                 - Could not get this to work - abandoned for now.
                hs -> [makeGS h (rc+1) | h <- getIsomorphismData (g, ic) hs]
                -- TODO: the above only filters graphs that unchanged by the rule application
                -- not those that are non-unique in the result set!
                -- hs -> [makeGS h (rc+1) | h <- getIsomorphismData (head hs, ic) $ tail hs]
                     where makeGS (x, y) z = GS x y z -}
evalSimpleCommand max ds c@(LoopedRuleCall rs) gs@(GS g rc) =
    if rc == max 
        then [Unfinished]
        else 
            case evalSimpleCommand max ds (RuleCall rs) gs of
                [Unfinished] -> [Unfinished]
                -- Loop terminates, return input GraphState
                [Failure] -> [gs]
                -- One rule call successful. If the bound has been reached, stop and return hs,
                -- otherwise continue with the loop.
                hs -> concatMap (evalSimpleCommand max ds c) hs
evalSimpleCommand max ds (ProcedureCall proc) gs = evalCommandSequence max (decls++ds) cs gs
    where Procedure id decls cs = procLookup proc ds
evalSimpleCommand max ds c@(LoopedProcedureCall proc) gs = 
    case evalSimpleCommand max ds (ProcedureCall proc) gs of
        [Unfinished] -> [Unfinished]
        -- Loop terminates, return input GraphState
        [Failure] -> [gs]
        hs     -> concatMap (evalSimpleCommand max ds c) hs
evalSimpleCommand max ds Skip (GS g rc) = [GS g (rc+1)]
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



