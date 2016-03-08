module OILR4.HostCompile (compileHostGraph) where

import OILR4.Instructions

import GPSyntax
import Graph
import Mapping

import Debug.Trace
import Data.List

data GraphElem = N NodeKey | E EdgeKey deriving (Eq, Ord, Show)
type GraphElemId = (RuleGraph, GraphElem)
type OilrCode = [Instr]

compileHostGraph :: HostGraph -> OilrCode
compileHostGraph g = nodes g ++ edges g

-- -------------------------------------------------------------------
-- host graph OILR instruction generation
-- -------------------------------------------------------------------

nodes :: HostGraph -> OilrCode
nodes g = concatMap node $ allNodes g
    where
        node (n, HostNode _ root (HostLabel [] c)) =
            ABN (nodeNumber n)
            : (if root then RBN (nodeNumber n) True else NOP)
            : (if c == Uncoloured then NOP else CBL (nodeNumber n) (definiteLookup c colourIds ) )
            : []


edges :: HostGraph -> OilrCode
edges g = map edge $ allEdges g
    where
        edge (e, _) = ABE (edgeNumber e) (nodeNumber $ source e) (nodeNumber $ target e)


