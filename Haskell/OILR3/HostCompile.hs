module OILR3.HostCompile (compileHostGraph) where

import OILR3.Instructions

import GPSyntax
import Graph
import Mapping

import Debug.Trace
import Data.List


data GraphElem = N NodeKey | E EdgeKey deriving (Eq, Ord, Show)
type GraphElemId = (RuleGraph, GraphElem)
type SemiOilrCode = [Instr GraphElemId GraphElemId]
type OilrCode = [Instr Int Int]

notImplemented n = error $ "Not implemented: " ++ show n

compileHostGraph :: HostGraph -> OilrCode
compileHostGraph g = nodes g ++ edges g

-- -------------------------------------------------------------------
-- host graph OILR instruction generation
-- -------------------------------------------------------------------

nodes :: HostGraph -> OilrCode
nodes g = concatMap node $ allNodes g
    where
        node (n, HostNode _ root (HostLabel [] Uncoloured)) = ADN (nodeNumber n) : (if root then RTN (nodeNumber n) : [] else [])


edges :: HostGraph -> OilrCode
edges g = map edge $ allEdges g
    where
        edge (e, _) = ADE (edgeNumber e) (nodeNumber $ source e) (nodeNumber $ target e)



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

