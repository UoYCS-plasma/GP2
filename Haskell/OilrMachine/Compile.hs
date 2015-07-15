module OilrMachine.Compile (compileGPProg, compileHostGraph) where

import OilrMachine.Instructions

import GPSyntax
import Graph

import Data.List

import Debug.Trace

notImplemented s = error $ "Not implemented: " ++ s

data Pred = None | Equ Int | GtE Int
type Signature = (Pred, Pred, Pred, Pred) -- (o, i, l, r)

type RegId = Int
type Travs = [(RegId, Trav)]
data Trav = Node | EdgeTo RegId | EdgeFrom | NoEdge

data Prog = Prog { ins :: [Instr] , t :: Int }


data PlanElem = Vertex NodeKey  -- Node and Edge already taken! 
               | Link NodeKey NodeKey
               | NoLink NodeKey NodeKey
    deriving Show

type Plan = (PlanElem, Plan)

searchPlans :: Rule -> [Plan]
searchPlans r@(Rule _ _ (lhs, _) _ _ cond) = []


-- convert a graph to a naive search plan
graphToPlan :: RuleGraph -> Plan
graphToPlan g = nodes ++ edges
    where
        nodes = map (\nk -> Vertex nk) $ allNodeKeys g
        edges = map (\ek -> Link ek) $ allEdgeKeys g

condsToPlan :: [Condition] -> Plan
condsToPlan NoCondition = []
condsToPlan c = error "Condition not yet supported"

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




compileGPProg = notImplemented "compileGPProg"
compileHostGraph = notImplemented "compileHostGraph"





-- data Rule = Rule RuleName [Variable] (RuleGraph, RuleGraph) NodeInterface 
--            EdgeInterface Condition deriving Show

compileRule :: Rule -> [Instr]
compileRule r@(Rule id [] (lhs, rhs) ni ei cond) = notImplemented "compileRule"
compileRule _ = error "Variables are not yet supported"


compileLHS :: Rule -> [Instr]
compileLHS r@(Rule _ _ (lhs, rhs) ni ei cond) = []
    where
        ns = map (characteriseNode r ni) $ allNodeKeys lhs
        es = map (characteriseEdge r ei) $ allEdgeKeys lhs

-- Graph modification
compileRHS :: Rule -> [Instr]
compileRHS (Rule _ _ (lhs, rhs) ni ei _) = []
    where
        eDeletion = lhsEdgesToDelete lhs ei 
        nDeletion = lhsNodesToDelete lhs ni
        -- TODO: node creation can potentially be avoided by reusing
        -- nodes that would otherwise be deleted
        nCreation = rhsNodesToCreate rhs ni
        eCreation = rhsEdgesToCreate rhs ei





characteriseRuleGraph :: RuleGraph -> NodeInterface -> Signature
characteriseRuleGraph g ni =
    foldr combineSignatures (None, None, None, None)
        $ map (characteriseNode g ni) $ allNodeKeys g


combineSignatures :: Signature -> Signature -> Signature
combineSignatures (o1, i1, l1, r1) (o2, i2, l2, r2) =
    ( combinePreds o1 o2 , combinePreds i1 i2 , combinePreds l1 l2 , combinePreds r1 r2 )

combinePreds :: Pred -> Pred -> Pred
combinePreds (Equ x) (Equ y) = Equ $ max x y
combinePreds (Equ x) (GtE y) = Equ $ max x y
combinePreds (GtE x) (Equ y) = Equ $ max x y
combinePreds (GtE x) (GtE y) = GtE $ max x y
combinePreds None    p       = p
combinePreds p       None    = p

{-
characteriseEdge :: RuleGraph -> EdgeInterface -> EdgeKey -> (NodeKey, NodeKey)
characteriseEdge 
-}
characteriseNode :: RuleGraph -> NodeInterface -> NodeKey -> Signature
characteriseNode g ni nk = (o, i, l, r)
    where
        pred = if nk `elem` map fst ni then GtE else Equ
        o = pred $ outCount g nk
        i = pred $ inCount g nk
        l = pred $ loopCount g nk
        r = isRoot g nk


loopCount :: RuleGraph -> NodeKey -> Int
loopCount g nk = length $ joiningEdges g nk nk

outCount :: RuleGraph -> NodeKey -> Int
outCount g nk = outdegree g nk - loopCount g nk

inCount :: RuleGraph -> NodeKey -> Int
inCount g nk = indegree g nk - loopCount g nk

isRoot :: RuleGraph -> NodeKey -> Pred
isRoot g nk = if r then Equ 1 else None
    where
        RuleNode _ r _ = nLabel g nk
        



lhsNodesToDelete :: RuleGraph -> NodeInterface -> [NodeKey]
lhsNodesToDelete lhs ni = allNodeKeys lhs \\ map fst ni

lhsEdgesToDelete :: RuleGraph -> EdgeInterface -> [EdgeKey]
lhsEdgesToDelete lhs ei = allEdgeKeys lhs \\ map fst ei


rhsNodesToCreate :: RuleGraph -> NodeInterface -> [NodeKey]
rhsNodesToCreate rhs ni = allNodeKeys rhs \\ map snd ni

rhsEdgesToCreate :: RuleGraph -> EdgeInterface -> [EdgeKey]
rhsEdgesToCreate rhs ei = allEdgeKeys rhs \\ map snd ei


compileSig :: Signature -> [Instr]
compileSig = notImplemented "compileSig"





