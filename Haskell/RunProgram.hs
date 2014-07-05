module RunProgram where

import ApplyRule
import GPSyntax


runProgram :: GPProgram -> HostGraph -> Int -> [HostGraph]
runProgram prog h k = evalMain decls (findMain decls) h
    where
        Program decls = prog
        
findMain :: [Declaration] -> Main
findMain ((MainDecl m):ds) = m
findMain (_:ds) = findMain ds
findMain [] = error "No main procedure defined."

evalMain :: [Declaration] -> Main -> HostGraph -> [HostGraph]
evalMain decls (Main coms) h = evalCommandSequence decls coms h

evalCommandSequence :: [Declaration] -> [Command] -> HostGraph -> [HostGraph]
evalCommandSequence decls [] h = [h]
evalCommandSequence decls (c:cs) h = 
    case evalCommand decls c h of
        [] -> []
        hs -> concatMap (evalCommandSequence decls cs) hs

evalCommand :: [Declaration] -> Command -> HostGraph -> [HostGraph]
evalCommand decls (Conditional cond) h = evalConditional decls cond h
evalCommand decls (ProgramOr c1 c2) h = evalCommandSequence decls c1 h
evalCommand decls (Loop commands) h = 
    case evalCommandSequence decls commands h of
        [] -> [h]
        hs -> concatMap (evalCommandSequence decls commands) hs
evalCommand decls (RuleCall (r:rs)) h = 
    case applyRule h $ ruleLookup r decls of
        [] -> evalCommand decls (RuleCall rs) h
        hs -> hs
evalCommand decls c@(LoopedRuleCall rs) h = 
    case evalCommand decls (RuleCall rs) h of
        [] -> [h]
        hs -> concatMap (evalCommand decls c) hs
-- localDecls are placed at the start of the new declaration list
-- so that procLookup and ruleLookup always return the correct rule
-- or procedure with respect to GP2's scoping rules.`
evalCommand decls (ProcedureCall proc) h = 
    evalCommandSequence (localDecls ++ decls) coms h
        where
        Procedure id localDecls coms = procLookup proc decls
evalCommand decls c@(LoopedProcedureCall proc) h = 
    case evalCommand decls (ProcedureCall proc) h of
        [] -> [h]
        hs -> concatMap (evalCommand decls c) hs
evalCommand decls Skip h = [h]
evalCommand decls Fail h = []
evalCommand decls (RuleCall []) h = []


evalConditional :: [Declaration] -> Conditional -> HostGraph -> [HostGraph]
evalConditional decls (IfStatement cond th el) h = 
    case evalCommandSequence decls cond h of 
        [] -> evalCommandSequence decls el h
        hs -> evalCommandSequence decls th h
evalConditional decls (TryStatement cond th el) h = 
    case evalCommandSequence decls cond h of
        [] -> evalCommandSequence decls el h
        hs -> concatMap (evalCommandSequence decls th) hs

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



