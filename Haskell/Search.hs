module Search where


import Prelude hiding (lookup)
import Data.List
import Data.Maybe

import ExAr
import GPGraph
import Graph
import GPSyntax


makeTestGraph n = nReLabel gr id (HostLabel [] Green)
    where
        gr = k n
        id = head $ allNodes gr

--makeRuleGraph n = makeTestGraph

testGraph = makeTestGraph 3
--searchFor = makeRuleGraph 2

type RuleNodeId = NodeId
type HostNodeId = NodeId
type RuleEdgeId = EdgeId
type HostEdgeId = EdgeId

type GraphMorphism = ( NodeMatches, EdgeMatches ) 
type NodeMatches = [ NodeMatch ]
type EdgeMatches = [ EdgeMatch ]
type NodeMatch = (RuleNodeId, HostNodeId)
type EdgeMatch = (RuleEdgeId, HostEdgeId)


-- two nodes are equal if their labels are equal
nodesMatch :: HostGraph -> RuleGraph -> HostNodeId -> RuleNodeId -> Bool
nodesMatch g r gn rn = nLabel g gn == nLabel r rn

-- two edges are equal if their labels are equal AND the nodes on either end are equal
edgesMatch :: HostGraph -> RuleGraph -> HostEdgeId -> RuleEdgeId -> Bool
edgesMatch g r ge re = ( eLabel g ge == eLabel r re )
                       && ( nodesMatch g r (fromJust $ source g ge) (fromJust $ source r re) )
                       && ( nodesMatch g r (fromJust $ target g ge) (fromJust $ target r re) )
       
matchRuleNode :: HostGraph -> RuleGraph -> RuleNodeId -> NodeMatches
matchRuleNode g r rn =
    [ (rn, n) | n <- allNodes g , nodesMatch g r n rn ]

matchRuleEdge :: HostGraph -> RuleGraph -> RuleEdgeId -> EdgeMatches
matchRuleEdge g r re =
    [ (re, e) | e <- allEdges g , edgesMatch g r e re ]

matchNodes :: HostGraph -> RuleGraph -> NodeMatches
matchNodes g r = concatMap ( matchRuleNode g r ) $ allNodes r

matchEdges :: HostGraph -> RuleGraph -> EdgeMatches
matchEdges g r = concatMap ( matchRuleEdge g r ) $ allEdges r
{-matchEdges g r (rn, gn) = union ins outs
    where
        ins  = filter (uncurry $ edgesMatch g r)
                    [ (re, e) | e <- inEdges g gn, re <- inEdges r rn ]
        outs = filter (uncurry $ edgesMatch g r)
                    [ (re, e) | e <- outEdges g gn, re <- outEdges r rn ] -}

matchGraph :: HostGraph -> RuleGraph -> [GraphMorphism]
matchGraph g r = [ (nm, em) | nm <- matchNodes g r , em <- matchEdges g r ]

                


