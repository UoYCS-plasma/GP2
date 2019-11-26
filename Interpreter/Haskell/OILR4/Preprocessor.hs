module OILR4.Preprocessor (ppProg) where

import GPSyntax
import Mapping

import Data.List

-- Simplify a GP2 program to make compilation easier


type ElemId = Integer
type Name = String

data IRDefn = IRule Name IRGraph
            | IProc Name [IRCall]

data IRGraph = IRGraph { iElems :: [IRElem]   -- "interface" elements (unmodified)
                       , dElems :: [IRElem]   -- deleted elements (LHS only)
                       , cElems :: [IRElem]   -- created elements (RHS only)
                       , mElems :: Mapping IRElem IRElem } -- modified elements

data IRCall = IRLoop Name
            | IRCall Name

data IRLabel = IRLabel Colour
    deriving (Show, Eq)

data IRElem = N ElemId IRLabel
            | E ElemId ElemId ElemId
            deriving (Show, Eq)


-- Get Nodes and edges from the LHS of the IRGraph
irLhs :: IRGraph -> ([IRElem], [IRElem])
irLhs (IRGraph is ds _ ms) = (irNodes lhs, irEdges lhs)
    where lhs = concat [ is, ds, dom ms ]

-- ...and from the right hand side
irRhs :: IRGraph -> ([IRElem], [IRElem])
irRhs (IRGraph is _ cs ms) = (irNodes rhs, irEdges rhs)
    where rhs = concat [ is, cs, rng ms ]

-- extract the nodes from a list of elements
irNodes :: [IRElem] -> [IRElem]
irNodes els = [ n | n@(N _ _) <- els ]

-- extract the edges from a list of elements
irEdges :: [IRElem] -> [IRElem]
irEdges els = [ e | e@(E _ _ _) <- els ]

-- -----------------------------------------------
-- Helper functions
-- -----------------------------------------------

data RuleElem = No RuleNode
              | Ed AstRuleEdge
              deriving (Show, Eq)
{-
-- TODO: this needs to assign the same ID to rhs edges as to
-- their corresponding lhs edge! Eek
assignElementIds :: AstRule -> Mapping RuleElem ElemId
assignElementIds (AstRule _ _ (lhs, rhs) cond) = (nub . concat) [lhsNodes, rhsMap]
    where (lhsNodes, n) = mapNodes 0 lhs 
          (lhsEdges, n') = mapEdges n lhs
          (rhsNodes, n'') = mapNodes n' rhs
  -}          


-- -----------------------------------------------
-- Pre-processing to IR
-- -----------------------------------------------




notImplemented n = error $ "Preproc not implemented " ++ show n

{-   A handy AST reference...

data HostEdge = HostEdge NodeName NodeName HostLabel deriving Show
data AstHostGraph = AstHostGraph [HostNode] [HostEdge] deriving Show
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

data Rule = Rule RuleName [Variable] (AstRuleGraph, AstRuleGraph) NodeInterface 
            EdgeInterface Condition deriving Show

data AstRule = AstRule RuleName [Variable] (AstRuleGraph, AstRuleGraph) 
               Condition  deriving Show
data AstRuleGraph = AstRuleGraph [RuleNode] [AstRuleEdge] deriving (Show,Eq)
data AstRuleEdge = AstRuleEdge EdgeName Bool NodeName NodeName RuleLabel deriving (Show, Eq)

data RuleNode = RuleNode NodeName Bool RuleLabel deriving (Show, Eq)
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

type Preprocessor a b = a -> [(a, b)]



ppProg :: Preprocessor GPProgram [IRDefn]
ppProg = notImplemented 10


ppDecl :: Preprocessor Declaration IRDefn
ppDecl = notImplemented 11



ppRule :: AstRule -> AstRule
ppRule r@(AstRule id vs (lhs, rhs) cs) = r

{-
ppLiftSeqs :: Command -> ([IRDefn], Block)
ppLiftSeqs (Block (ComSeq cs))       = (defs, Block (SimpleCommand (ProcedureCall defName)))
ppLiftSeqs (Block (LoopedComSeq cs)) = (defs, Block (SimpleCommand (LoopedProcedureCall defName)))
ppLiftSeqs (Block (ProgramOr a b)) = []
-}

