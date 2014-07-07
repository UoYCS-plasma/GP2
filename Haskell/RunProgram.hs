module RunProgram where

import ApplyRule
import GPSyntax


runProgram :: GPProgram -> HostGraph -> Int -> [HostGraph]
runProgram prog h k = evalMain ds (findMain ds) h
    where
        Program ds = prog
        
findMain :: [Declaration] -> Main
findMain ((MainDecl m):ds) = m
findMain (_:ds) = findMain ds
findMain [] = error "No main procedure defined."

evalMain :: [Declaration] -> Main -> HostGraph -> [HostGraph]
evalMain ds (Main coms) h = evalCommandSequence ds coms h

evalCommandSequence :: [Declaration] -> [Command] -> HostGraph -> [HostGraph]
evalCommandSequence ds [] h = [h]
evalCommandSequence ds (c:cs) h = 
    case evalCommand ds c h of
        [] -> []
        hs -> concatMap (evalCommandSequence ds cs) hs

evalCommand :: [Declaration] -> Command -> HostGraph -> [HostGraph]
evalCommand ds (Block b) h = evalBlock ds b h
evalCommand ds (IfStatement cond th el) h = 
    case evalBlock ds cond h of 
        [] -> evalBlock ds el h
        hs -> evalBlock ds th h
evalCommand ds (TryStatement cond th el) h = 
    case evalBlock ds cond h of
        [] -> evalBlock ds el h
        hs -> concatMap (evalBlock ds th) hs

evalBlock :: [Declaration] -> Block -> HostGraph -> [HostGraph]
evalBlock ds (ComSeq cs) h = evalCommandSequence ds cs h
evalBlock ds (LoopedComSeq cs) h = 
    case evalCommandSequence ds cs h of
        [] -> []
        hs -> concatMap (evalCommandSequence ds cs) hs
evalBlock ds (SimpleCommand sc) h = evalSimpleCommand ds sc h
evalBlock ds (ProgramOr b1 b2) h = evalBlock ds b1 h





evalSimpleCommand :: [Declaration] -> SimpleCommand -> HostGraph -> [HostGraph]
evalSimpleCommand ds (RuleCall (r:rs)) h = 
    case applyRule h $ ruleLookup r ds of
        [] -> evalSimpleCommand ds (RuleCall rs) h
        hs -> hs
evalSimpleCommand ds (RuleCall []) h = []
evalSimpleCommand ds c@(LoopedRuleCall rs) h = 
    case evalSimpleCommand ds (RuleCall rs) h of
        [] -> [h]
        hs -> concatMap (evalSimpleCommand ds c) hs
evalSimpleCommand ds (ProcedureCall proc) h = evalCommandSequence (decls++ds) cs h
    where
        Procedure id decls cs = procLookup proc ds
evalSimpleCommand ds c@(LoopedProcedureCall proc) h = 
    case evalSimpleCommand ds (ProcedureCall proc) h of
        [] -> [h]
        hs -> concatMap (evalSimpleCommand ds c) hs
evalSimpleCommand ds Skip h = [h]
evalSimpleCommand ds Fail h = []


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



