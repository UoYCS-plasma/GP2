module OilrMachine.Compile (compileGPProg, compileHostGraph) where

import OilrMachine.Instructions

import GPSyntax
import Graph

import Data.List

import Debug.Trace

notImplemented s = error $ "Not implemented: " ++ s

data Pred = None | Equ Int | GtE Int
type Signature = (Pred, Pred, Pred, Bool) -- (o, i, l, r)

type RegId = Int
type Travs = [(RegId, Trav)]
data Trav = Node | EdgeTo RegId | EdgeFrom | NoEdge



compileGPProg = notImplemented "compileGPProg"
compileHostGraph = notImplemented "compileHostGraph"





-- data Rule = Rule RuleName [Variable] (RuleGraph, RuleGraph) NodeInterface 
--            EdgeInterface Condition deriving Show

compileRule :: Rule -> [Instr]
compileRule (Rule id [] (lhs, rhs) ni ei cond) = notImplemented "compileRule"
compileRule _ = error "Variables are not yet supported"






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

isRoot :: RuleGraph -> NodeKey -> Bool
isRoot g nk = r
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





