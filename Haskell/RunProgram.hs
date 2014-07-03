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
evalMain decls (Main b) h = evalBlock decls b h

evalBlock :: [Declaration] -> Command -> HostGraph -> [HostGraph]
evalBlock decls (Sequence (b:bs)) h = case evalBlock decls b h of
    [] -> []
    hs -> concatMap (evalBlock decls (Sequence bs)) hs
evalBlock decls (Sequence []) h = [h]
evalBlock decls (IfStatement cond th el) h = case evalBlock decls cond h of 
    [] -> evalBlock decls el h
    hs -> evalBlock decls th h
evalBlock decls (TryStatement cond th el) h = case evalBlock decls cond h of
    [] -> evalBlock decls el h
    hs -> concatMap (evalBlock decls th) hs
evalBlock decls (ProgramOr b1 b2) h = evalBlock decls b1 h
evalBlock decls (Loop block) h = case evalBlock decls block h of
    [] -> [h]
    hs -> concatMap (evalBlock decls block) hs
evalBlock decls (SimpleCommand s) h = evalSimpleCommand decls s h

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
        p id (RuleDecl (Rule name _ _ _ _ _)) = id == name 
        p id _ = False


evalSimpleCommand :: [Declaration] -> SimpleCommand -> HostGraph -> [HostGraph]
evalSimpleCommand ds (RuleCall (r:rs)) h = case applyRule h $ ruleLookup r ds of
    [] -> evalSimpleCommand ds (RuleCall rs) h
    hs -> hs
evalSimpleCommand ds c@(LoopedRuleCall rs) h = case evalSimpleCommand ds (RuleCall rs) h of
    [] -> [h]
    hs -> concatMap (evalSimpleCommand ds c) hs
evalSimpleCommand ds (ProcedureCall proc) h = evalBlock (decls++ds) block h
    where
        Procedure id decls block = procLookup proc ds
evalSimpleCommand ds c@(LoopedProcedureCall proc) h = case evalSimpleCommand ds (ProcedureCall proc) h of
    [] -> [h]
    hs -> concatMap (evalSimpleCommand ds c) hs
evalSimpleCommand ds Skip h = [h]
evalSimpleCommand ds Fail h = []
evalSimpleCommand ds (RuleCall []) h = []


