module OILR3.HostCompile (compileHostGraph, compileProgram) where

import OILR3.Instructions

import GPSyntax
import Graph

import Debug.Trace

type OilrProg = [Instr]


notImplemented n = error $ "Not implemented: " ++ show n


compileHostGraph :: HostGraph -> OilrProg
compileHostGraph g = nodes g ++ edges g

compileProgram :: GPProgram -> OilrProg
compileProgram (Program ds) = concatMap oilrCompileDeclaration ds

-- -------------------------------------------------------------------
-- host graph OILR instruction generation
-- -------------------------------------------------------------------

nodes :: HostGraph -> OilrProg
nodes g = concatMap node $ allNodes g
    where
        node (n, HostNode _ root (HostLabel [] Uncoloured)) = ADN : (if root then RTN (nodeNumber n) : [] else [])


edges :: HostGraph -> OilrProg
edges g = map edge $ allEdges g
    where
        edge (e, _) = ADE (nodeNumber $ source e) (nodeNumber $ target e)


-- -------------------------------------------------------------------
-- program OILR instruction generation
-- -------------------------------------------------------------------

{-   A handy AST reference...

data GPProgram = Program [Declaration] deriving Show

data Declaration = MainDecl Main
                 | ProcDecl Procedure
                 | AstRuleDecl AstRule
                 | RuleDecl Rule
     deriving Show

data Main = Main [Command] deriving Show

data Procedure = Procedure ProcName [Declaration] [Command] deriving Show

data Command = Block Block
             | IfStatement Block Block Block 
             | TryStatement Block Block Block
    deriving Show

data Block = ComSeq [Command]
           | LoopedComSeq [Command]
           | SimpleCommand SimpleCommand
           | ProgramOr Block Block      
    deriving (Show)
      
data SimpleCommand = RuleCall [RuleName]
                   | LoopedRuleCall [RuleName]
                   | ProcedureCall ProcName
                   | LoopedProcedureCall ProcName
                   | Skip
                   | Fail
    deriving Show

data Rule = Rule RuleName [Variable] (RuleGraph, RuleGraph) NodeInterface 
            EdgeInterface Condition deriving Show

data Condition = NoCondition
               | TestInt VarName
               | TestChr VarName
               | TestStr VarName
               | TestAtom VarName
               | Edge NodeName NodeName (Maybe RuleLabel)
               | Eq GPList GPList
               | NEq GPList GPList
               | Greater RuleAtom RuleAtom
               | GreaterEq RuleAtom RuleAtom
               | Less RuleAtom RuleAtom
               | LessEq RuleAtom RuleAtom
               | Not Condition
               | Or Condition Condition
               | And Condition Condition
    deriving Show


-}


oilrCompileDeclaration :: Declaration -> OilrProg
oilrCompileDeclaration (MainDecl m) = oilrCompileMain m
oilrCompileDeclaration (ProcDecl p) = oilrCompileProc p
oilrCompileDeclaration (RuleDecl r) = oilrCompileRule r

oilrCompileMain (Main cs) = (DEF "Main" : concatMap oilrCompileCommand cs) ++ [END]

oilrCompileProc (Procedure name ds cs) = notImplemented 1

oilrCompileCommand (Block b) = oilrCompileBlock b
oilrCompileCommand (IfStatement  cn th el) = notImplemented 2
oilrCompileCommand (TryStatement cn th el) = notImplemented 3

oilrCompileBlock (ComSeq cs)       = notImplemented 4
oilrCompileBlock (LoopedComSeq cs) = notImplemented 5
oilrCompileBlock (SimpleCommand s) = oilrCompileSimple s
oilrCompileBlock (ProgramOr a b)   = notImplemented 6

oilrCompileSimple (RuleCall      [r]) = [ CAL r ]
oilrCompileSimple (LoopedRuleCall [r]) = [ ALP r ]
oilrCompileSimple (RuleCall       rs) = notImplemented 7 -- non-deterministic choice(?)
oilrCompileSimple (LoopedRuleCall rs) = notImplemented 8
oilrCompileSimple (ProcedureCall       p) = [ CAL p ]
oilrCompileSimple (LoopedProcedureCall p) = [ ALP p ]
oilrCompileSimple Skip   = [ TRU , RET ]
oilrCompileSimple Fail   = [ FLS , RET ]



-- -------------------------------------------------------------------
-- rule compilation is complex...
-- -------------------------------------------------------------------

data OilrRuleProp = NodesAdded | NodesDeleted | EdgesAdded | EdgesDeleted deriving Show


-- Return the lists of (deleted, created) node ids

analyseNodeInterface :: Rule -> ([NodeKey], [NodeKey])
analyseNodeInterface (Rule _ _ (lhs, rhs) nif _ _) = 
    ( [ lnk | lnk <- allNodeKeys lhs , not $ lnk `elem` lhsInterfaceNodes ]
    , [ rnk | rnk <- allNodeKeys rhs , not $ rnk `elem` rhsInterfaceNodes ] )
    where
        lhsInterfaceNodes = map fst nif
        rhsInterfaceNodes = map snd nif

analyseEdgeInterface :: Rule -> ([EdgeKey], [EdgeKey])
analyseEdgeInterface (Rule _ _ (lhs, rhs) _ eif _) =
    ( [ lek | lek <- allEdgeKeys lhs, not $ lek `elem` lhsInterfaceEdges ]
    , [ rek | rek <- allEdgeKeys rhs, not $ rek `elem` rhsInterfaceEdges ] )
    where
        lhsInterfaceEdges = map fst eif
        rhsInterfaceEdges = map snd eif
    
analyseRule :: Rule -> ([NodeKey], [EdgeKey], [NodeKey], [EdgeKey])
analyseRule r = (delNodes, delEdges, newNodes, newEdges)
    where
        (delNodes, newNodes) = analyseNodeInterface r
        (delEdges, newEdges) = analyseEdgeInterface r


oilrCompileRule r@(Rule name _ _ _ _ _) = [DEF name] ++ body ++ [END]
    where
        body = case analyseRule r of
            ([], [], [], []) -> oilrCompilePredicate r
            _ -> notImplemented 16

-- This alias may change -- there could be optimisations applicable to predicates that aren't to arbitrary left-hand-sides.
oilrCompilePredicate = oilrCompileLhs


oilrCompileLhs = notImplemented 10


oilrCompileRhs = notImplemented 11


oilrCompileCondition NoCondition = []
oilrCompileCondition _ = notImplemented 12

