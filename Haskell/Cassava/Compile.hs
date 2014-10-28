module Cassava.Compile (compileGPProg) where

import GPSyntax
import Cassava.Instructions

notImplemented = error "Not implemented"


compileGPProg :: GPProgram -> Prog
compileGPProg (Program ds) = concatMap compileDecl ds


compileDecl :: Declaration -> Prog
compileDecl (MainDecl m) = compileMain m
compileDecl (ProcDecl p) = compileProc p
compileDecl (RuleDecl r) = compileRule r
compileDecl _ = notImplemented

compileMain :: Main -> Prog
compileMain (Main cs) = concatMap compileComm cs

compileProc :: Procedure -> Prog
compileProc (Procedure _ ds cs) = concatMap compileDecl ds ++ concatMap compileComm cs

compileComm :: Command -> Prog
compileComm (Block b) = compileBlock b
compileComm (IfStatement _ _ _) = notImplemented
compileComm (TryStatement _ _ _) = notImplemented

compileBlock :: Block -> Prog
compileBlock (ComSeq cs) = concatMap compileComm cs
compileBlock (LoopedComSeq _) = notImplemented
compileBlock (SimpleCommand s) = compileSimple s
compileBlock (ProgramOr _ _) = notImplemented


compileSimple :: SimpleCommand -> Prog
compileSimple (RuleCall rs) = notImplemented
compileSimple (LoopedRuleCall rs) = notImplemented
compileSimple (ProcedureCall p) = notImplemented
compileSimple (LoopedProcedureCall p) = notImplemented
compileSimple Skip = notImplemented
compileSimple Fail = notImplemented

compileRule :: Rule -> Prog
compileRule (Rule _ _ (lhs, rhs) nodeIf edgeIf cond) =
    compileCond cond
    ++ compileLhs nodeIf lhs
    ++ compileRhs rhs

compileCond :: Condition -> Prog
compileCond NoCondition = []
compileCond _ = notImplemented

compileLhs :: NodeInterface -> RuleGraph -> Prog
compileLhs nif g = notImplemented

compileRhs :: RuleGraph -> Prog
compileRhs = notImplemented

