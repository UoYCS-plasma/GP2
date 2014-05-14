module Search where

import Prelude hiding (lookup)
import Data.List
import Data.Maybe

import ExAr
import GP2Graph
import Graph

makeTestGraph n = nReLabel gr id (GP2Label [] Green)
    where
        gr = k n
        id = head $ allNodes gr

makeRuleGraph n = nReLabel gr id (GP2Label [] Green)
    where
        gr = k n
        id = head $ allNodes gr

testGraph = makeTestGraph 3
searchFor = makeRuleGraph 2


type GP2RuleGraph = GP2Graph
type GP2RuleLabel = GP2Label
type GraphMorphism = ([NodeId], [EdgeId])
type RuleMatch = (NodeId, [NodeId])



-- two nodes are equal if their labels are equal
nodesAreEqual :: GP2Graph -> GP2RuleGraph -> NodeId -> NodeId -> Bool
nodesAreEqual g r gn rn = nLabel g gn == nLabel r rn

-- two edges are equal if their labels are equal AND the nodes on either end are equal
edgesAreEqual :: GP2Graph -> GP2RuleGraph -> EdgeId -> EdgeId -> Bool
edgesAreEqual g r ge re = ( eLabel g ge == eLabel r re )
                       && ( nodesAreEqual g r (fromJust $ source g ge) (fromJust $ source r re) )
                       && ( nodesAreEqual g r (fromJust $ target g ge) (fromJust $ target r re) )
       

matchRuleNode :: GP2Graph -> GP2RuleGraph -> NodeId -> RuleMatch
matchRuleNode g r rn =
    ( rn, [ n | n <- allNodes g , nodesAreEqual g r n rn ] )

matchNodes :: GP2Graph -> GP2RuleGraph -> [RuleMatch]
matchNodes g r = map ( matchRuleNode g r ) $ allNodes r

matchEdges :: GP2Graph -> GP2RuleGraph -> RuleMatch -> [EdgeId]
matchEdges g r (rn, gns) = union ins outs
    where
        ins  = intersectBy (edgesAreEqual g r)
                    [ e | e <- concatMap (inEdges g) gns ]
                    [ re | re <- inEdges r rn ]
        outs = intersectBy (edgesAreEqual g r)
                    [ e | e <- concatMap (outEdges g) gns ]
                    [ re | re <- outEdges r rn ]

matchGraph :: GP2Graph -> GP2RuleGraph -> [GraphMorphism]
matchGraph g r = zip (map snd node_matches) edge_matches
    where
        node_matches = matchNodes g r
        edge_matches = map (matchEdges g r) node_matches
                


