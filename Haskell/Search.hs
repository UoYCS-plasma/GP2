module Search where

import Prelude hiding (lookup)
import Data.List
import Data.Maybe

import ExAr
import GPGraph
import qualified Graph
import GPSyntax


source = Graph.source
target = Graph.target
eLabel = Graph.eLabel
nLabel = Graph.nLabel
allNodes = Graph.allNodes
allEdges = Graph.allEdges
nReLabel = Graph.nReLabel


makeTestGraph n = nReLabel gr id (GPHostLabel [] Green)
    where
        gr = k n
        id = head $ allNodes gr

makeRuleGraph = makeTestGraph

testGraph = makeTestGraph 3
searchFor = makeRuleGraph 2

type GraphRuleNodeId = Graph.NodeId
type GraphHostNodeId = Graph.NodeId

type GraphRuleEdgeId = Graph.EdgeId
type GraphHostEdgeId = Graph.EdgeId

type GPRuleLabel = GPHostLabel

type GraphMorphism = ( NodeMatches, EdgeMatches ) 
type NodeMatches = [ NodeMatch ]
type EdgeMatches = [ EdgeMatch ]
type NodeMatch = (GraphRuleNodeId, GraphHostNodeId)
type EdgeMatch = (GraphRuleEdgeId, GraphHostEdgeId)


-- two nodes are equal if their labels are equal
nodesMatch :: GPHostGraph -> GPRuleGraph -> HostNodeId -> RuleNodeId -> Bool
nodesMatch g r gn rn = nLabel g gn == nLabel r rn

-- two edges are equal if their labels are equal AND the nodes on either end are equal
edgesMatch :: GPHostGraph -> GPRuleGraph -> GraphHostEdgeId -> GraphRuleEdgeId -> Bool
edgesMatch g r ge re = ( eLabel g ge == eLabel r re )
                       && ( nodesMatch g r (fromJust $ source g ge) (fromJust $ source r re) )
                       && ( nodesMatch g r (fromJust $ target g ge) (fromJust $ target r re) )
       
matchRuleNode :: GPHostGraph -> GPRuleGraph -> GraphRuleNodeId -> NodeMatches
matchRuleNode g r rn =
    [ (rn, n) | n <- allNodes g , nodesMatch g r n rn ]

matchRuleEdge :: GPHostGraph -> GPRuleGraph -> GraphRuleEdgeId -> EdgeMatches
matchRuleEdge g r re =
    [ (re, e) | e <- allEdges g , edgesMatch g r e re ]

matchNodes :: GPHostGraph -> GPRuleGraph -> NodeMatches
matchNodes g r = concatMap ( matchRuleNode g r ) $ allNodes r

matchEdges :: GPHostGraph -> GPRuleGraph -> EdgeMatches
matchEdges g r = concatMap ( matchRuleEdge g r ) $ allEdges r
{-matchEdges g r (rn, gn) = union ins outs
    where
        ins  = filter (uncurry $ edgesMatch g r)
                    [ (re, e) | e <- inEdges g gn, re <- inEdges r rn ]
        outs = filter (uncurry $ edgesMatch g r)
                    [ (re, e) | e <- outEdges g gn, re <- outEdges r rn ] -}

matchGraph :: GPHostGraph -> GPRuleGraph -> [GraphMorphism]
matchGraph g r = [ (nm, em) | nm <- matchNodes g r , em <- matchEdges g r ]

                


