module Search where

import Prelude hiding (lookup)
import Data.List
import Data.Maybe

import ExAr
import GP2Graph
import Graph

makeTestGraph n = nReLabel gr id (GP2HostLabel [] Green)
    where
        gr = k n
        id = head $ allNodes gr

makeRuleGraph = makeTestGraph

testGraph = makeTestGraph 3
searchFor = makeRuleGraph 2


type GP2RuleGraph = GP2Graph
type GP2RuleLabel = GP2HostLabel

type GraphMorphism = ( NodeMatches, EdgeMatches ) 
type NodeMatches = [ NodeMatch ]
type EdgeMatches = [ EdgeMatch ]
type NodeMatch = (NodeId, NodeId)
type EdgeMatch = (EdgeId, EdgeId)


-- two nodes are equal if their labels are equal
nodesMatch :: GP2Graph -> GP2RuleGraph -> NodeId -> NodeId -> Bool
nodesMatch g r gn rn = nLabel g gn == nLabel r rn

-- two edges are equal if their labels are equal AND the nodes on either end are equal
edgesMatch :: GP2Graph -> GP2RuleGraph -> EdgeId -> EdgeId -> Bool
edgesMatch g r ge re = ( eLabel g ge == eLabel r re )
                       && ( nodesMatch g r (fromJust $ source g ge) (fromJust $ source r re) )
                       && ( nodesMatch g r (fromJust $ target g ge) (fromJust $ target r re) )
       
matchRuleNode :: GP2Graph -> GP2RuleGraph -> NodeId -> NodeMatches
matchRuleNode g r rn =
    [ (rn, n) | n <- allNodes g , nodesMatch g r n rn ]

matchRuleEdge :: GP2Graph -> GP2RuleGraph -> EdgeId -> EdgeMatches
matchRuleEdge g r re =
    [ (re, e) | e <- allEdges g , edgesMatch g r e re ]

matchNodes :: GP2Graph -> GP2RuleGraph -> NodeMatches
matchNodes g r = concatMap ( matchRuleNode g r ) $ allNodes r

matchEdges :: GP2Graph -> GP2RuleGraph -> EdgeMatches
matchEdges g r = concatMap ( matchRuleEdge g r ) $ allEdges r
{-matchEdges g r (rn, gn) = union ins outs
    where
        ins  = filter (uncurry $ edgesMatch g r)
                    [ (re, e) | e <- inEdges g gn, re <- inEdges r rn ]
        outs = filter (uncurry $ edgesMatch g r)
                    [ (re, e) | e <- outEdges g gn, re <- outEdges r rn ] -}

matchGraph :: GP2Graph -> GP2RuleGraph -> [GraphMorphism]
matchGraph g r = [ (nm, em) | nm <- matchNodes g r , em <- matchEdges g r ]

                


