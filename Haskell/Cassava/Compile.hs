module Cassava.Compile (compileGPProg) where

import GPSyntax
import Cassava.Instructions

notImplemented s = error $ "Not implemented: " ++ s


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
compileBlock (LoopedComSeq _) = notImplemented "compileBlock"
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
    (PROC id) :
    compileCond cond
    ++ compileLhs nodeIf lhs'
    ++ compileRhs nodeIf rhs'
    ++ [RET]
    where
        (nodeIf, edgeIf) = computeInterface lhs rhs
        (lhs', rhs') = sortNodes lhs rhs

-- TODO: replace null sort function with one that puts the most informative first.
sortNodes :: AstRuleGraph -> AstRuleGraph -> (AstRuleGraph, AstRuleGraph)
sortNodes lhs rhs = (lhs, rhs)

compileCond :: Condition -> Prog
compileCond NoCondition = []
compileCond _ = notImplemented "compileCond"

compileLhs :: NodeInterface -> AstRuleGraph -> Prog
compileLhs nif (AstRuleGraph ns es) = concatMap compileLhsNode ns

compileLhsNode :: RuleNode -> Prog
compileLhsNode n = [ NFI ins, NCO outs, NCL loops, NEXT, JF (-1) ]
    where
        ins   = countInEdges n
        outs  = countOutEdges n
        loops = countLoops n

compileLhsINode :: RuleNode -> Prog
compileLhsINode n = [ INFI ins, NCO outs, NCL loops, NEXT ]
    where
        ins   = countInEdges n
        outs  = countOutEdges n
        loops = countLoops n

-- TODO: actually compile RHS
compileRhs :: NodeInterface -> AstRuleGraph -> Prog
compileRhs _ _ = []


compileRhsDelNode :: RuleNode -> Prog
compileRhsDelNode _ = notImplemented "compileRhsDelNode"

-- TODO: vast oversimplification
countInEdges :: RuleNode -> Deg
countInEdges _ = 0

countOutEdges :: RuleNode -> Deg
countOutEdges _ = 0

countLoops :: RuleNode -> Deg
countLoops _ = 0

computeInterface :: AstRuleGraph -> AstRuleGraph -> (NodeInterface, EdgeInterface)
computeInterface = notImplemented "computeInterface"

