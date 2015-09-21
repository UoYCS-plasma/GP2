module RunProgram where

import Debug.Trace
import ApplyRule
import Graph (emptyGraph)
import GraphIsomorphism
import GPSyntax

-- GS represents a host-graph during program execution
-- and the number of rules applied to reach it.
-- We need rule application counts for failures because some 
-- conditional and looped computations continue after a Failure
data GraphState = GS HostGraph Int | Failure Int | Unfinished deriving Show

updAppCount :: (Int->Int) -> GraphState -> GraphState
updAppCount f (GS hg rc)    =  GS hg (f rc)
updAppCount f (Failure rc)  =  Failure (f rc)
updAppCount f Unfinished    =  Unfinished

-- A host graph and its isomorphism count.				
type GraphData = (HostGraph, Int) 
 
-- The output of the GP 2 interpreter is a list of output graphs, each with an 
-- isomorphism count; a failure count, and an unfinished execution count with
-- respect to the bound on rule applications. 
type Result = ([GraphData], Int, Int, (Int, Int))

runProgram :: GPProgram -> Int -> HostGraph -> Result
runProgram (Program ds) max g = resultWith isomorphismCount $ evalMain max ds (findMain ds) g

-- For use with --one commandline flag: only get first result
nSolutions :: Int -> GPProgram -> Int -> HostGraph -> Result
nSolutions n (Program ds) max g = resultWith (flip zip [1,1..]) $ take n $ evalMain max ds (findMain ds) g

resultWith :: ([HostGraph] -> [(HostGraph,Int)]) -> [GraphState] -> Result
resultWith f gs = let (gs',fc',uc',bds') = foldl add ([], 0, 0, (maxBound, 0)) gs in
    (f gs',fc',uc',bds')
    where
    add (gs, fc, uc, (low, high)) (GS g rc)    = (g:gs, fc, uc, (min rc low, max rc high))
    add (gs, fc, uc, bds)         Unfinished   = (gs, fc, uc+1, bds)
    add (gs, fc, uc, (low, high)) (Failure rc) = (gs, fc+1, uc, (min rc low, max rc high))
        
findMain :: [Declaration] -> Main
findMain ((MainDecl m):ds) = m
findMain (_:ds) = findMain ds
findMain [] = error "No main procedure defined."

evalMain :: Int -> [Declaration] -> Main -> HostGraph -> [GraphState]
evalMain max ds (Main coms) g = evalExprSeq max ds coms (GS g 0)

evalExprSeq :: Int -> [Declaration] -> [Expr] -> GraphState -> [GraphState]
evalExprSeq _ _ _ (Failure rc) = [Failure rc]
evalExprSeq _ _ _ Unfinished = [Unfinished]
evalExprSeq max ds [] gs = [gs]
evalExprSeq max ds (c:cs) gs =
    concatMap handleExprSeq $ evalExpr max ds c gs
    where handleExprSeq Unfinished   = [Unfinished]
          handleExprSeq (Failure rc) = [Failure rc]
          handleExprSeq gs'          = evalExprSeq max ds cs gs'

{-
data Expr = IfStatement Expr Expr Expr
          | TryStatement Expr Expr Expr
          | Looped Expr
          | Sequence [Expr]
          | ProcedureCall ProcName
          | RuleSet [RuleName]
          | Skip
          | Fail
          deriving (Show, Eq)
          -}
evalExpr :: Int -> [Declaration] -> Expr -> GraphState -> [GraphState]
evalExpr _ _ _ (Failure rc)  = [Failure rc]
evalExpr _ _ _ Unfinished    = [Unfinished]
evalExpr max ds (IfStatement cond thn els) gs = 
    concatMap handleIf $ evalExpr max ds cond gs
    where handleIf Unfinished = [Unfinished]
          handleIf (Failure rc) = evalExpr max ds els $ updAppCount (+rc) gs
          handleIf _            = evalExpr max ds thn gs
evalExpr max ds (TryStatement cond thn els) gs = 
    concatMap handleTry $ evalExpr max ds cond gs
    where handleTry Unfinished   = [Unfinished]
          handleTry (Failure rc) = evalExpr max ds els $ updAppCount (+rc) gs
          handleTry gs'          = evalExpr max ds thn gs'
evalExpr max ds (Looped e) gs = concatMap handleLoop $ evalExpr max ds e gs
    where handleLoop Unfinished = [Unfinished]
          handleLoop (Failure rc) = [updAppCount (const rc) gs]
          handleLoop gs' = evalExpr max ds (Looped e) gs'
evalExpr max ds (ProgramOr a b) gs = evalExpr max ds a gs ++ evalExpr max ds b gs
evalExpr max ds (RuleSet rs) (GS g rc) =
    if rc == max then [Unfinished]
    else case [ h | r <- rs , h <- applyRule g $ ruleLookup r ds ] of
            [] -> [Failure rc]
            hs -> [GS h (rc+1) | h <- hs]
evalExpr max ds (Sequence es) gs = evalExprSeq max ds es gs
evalExpr max ds (ProcedureCall proc) gs = evalExprSeq max (decls++ds) cs gs
    where Procedure id decls cs = procLookup proc ds


{-
evalCommandSequence :: Int -> [Declaration] -> [Command] -> GraphState -> [GraphState]
evalCommandSequence _ _ _ (Failure rc) = [Failure rc]
evalCommandSequence _ _ _ Unfinished = [Unfinished]
evalCommandSequence max ds [] gs = [gs]
evalCommandSequence max ds (c:cs) gs =
    concatMap handleCommSeq $ evalCommand max ds c gs
    where handleCommSeq Unfinished   = [Unfinished]
          handleCommSeq (Failure rc) = [Failure rc]
          handleCommSeq gs'          = evalCommandSequence max ds cs gs'

evalCommand :: Int -> [Declaration] -> Command -> GraphState -> [GraphState]
evalCommand _ _ _ (Failure rc)  = [Failure rc]
evalCommand _ _ _ Unfinished    = [Unfinished]
evalCommand max ds (Block b) gs = evalBlock max ds b gs 
evalCommand max ds (IfStatement cond pass fail) gs = 
    concatMap handleIf $ evalBlock max ds cond gs
    where handleIf Unfinished   = [Unfinished]
          handleIf (Failure rc) = evalBlock max ds fail $ updAppCount (+rc) gs
          handleIf _            = evalBlock max ds pass gs
evalCommand max ds (TryStatement cond pass fail) gs = 
    concatMap handleTry $ evalBlock max ds cond gs
    where handleTry Unfinished   = [Unfinished]
          handleTry (Failure rc) = evalBlock max ds fail $ updAppCount (+rc) gs
          handleTry gs'        = evalBlock max ds pass gs'

evalBlock :: Int -> [Declaration] -> Block -> GraphState -> [GraphState]
evalBlock _ _ _ (Failure rc) = [Failure rc]
evalBlock _ _ _ Unfinished = [Unfinished]
evalBlock max ds (ComSeq cs) gs = evalCommandSequence max ds cs gs
evalBlock max ds ls@(LoopedComSeq cs) gs@(GS g rc) =  
    concatMap handleLoop $ evalCommandSequence max ds cs gs
    where handleLoop Unfinished   = [Unfinished]
          handleLoop (Failure rc) = [updAppCount (const rc) gs]
          handleLoop gs'          = evalBlock max ds ls gs'
evalBlock max ds (SimpleCommand sc) gs = evalSimpleCommand max ds sc gs
evalBlock max ds (ProgramOr b1 b2) gs = evalBlock max ds b1 gs  ++ evalBlock max ds b2 gs

evalSimpleCommand :: Int -> [Declaration] -> SimpleCommand -> GraphState -> [GraphState]
evalSimpleCommand _ _ _ (Failure rc) = [Failure rc]
evalSimpleCommand _ _ _ Unfinished = [Unfinished]
evalSimpleCommand max ds (RuleCall rs) (GS g rc) = 
    if rc == max then [Unfinished]
    else case [h | r <- rs, h <- applyRule g $ ruleLookup r ds] of
         [] -> [Failure rc]
         hs -> [GS h (rc+1) | h <- hs]
evalSimpleCommand max ds c@(LoopedRuleCall rs) gs = 
    evalLooped max ds (RuleCall rs) gs
evalSimpleCommand max ds (ProcedureCall proc) gs = evalCommandSequence max (decls++ds) cs gs
    where Procedure id decls cs = procLookup proc ds
evalSimpleCommand max ds c@(LoopedProcedureCall proc) gs = 
    evalLooped max ds (ProcedureCall proc) gs 
evalSimpleCommand max ds Skip (GS g rc) = [GS g rc]
evalSimpleCommand max ds Fail (GS _ rc) = [Failure rc]


evalLooped :: Int -> [Declaration] -> SimpleCommand -> GraphState -> [GraphState]
evalLooped max ds c gs  =
    [ gsFinal 
    | gsInter <- evalSimpleCommand max ds c gs,
      gsFinal <- case gsInter of Unfinished -> [Unfinished]
                                 Failure rc -> [updAppCount (const rc) gs] -- NB!
                                 _          -> evalLooped max ds c gsInter ]
-}
procLookup :: ProcName -> [Declaration] -> Procedure
procLookup id decls = case matches of
   []  -> error $ "Reference to undefined procedure " ++ id
   d:_ -> d
   where
   matches = [d | ProcDecl d@(Procedure name _ _) <- decls, name == id]

ruleLookup :: RuleName -> [Declaration] -> Rule
ruleLookup id decls = case matches of
   []  -> error $ "Reference to undefined rule " ++ id
   d:_ -> d
   where
   matches = [d | RuleDecl d@(Rule name _ _ _ _ _) <- decls, name == id]

