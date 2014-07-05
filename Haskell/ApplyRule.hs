module ApplyRule where

import Data.List
import Data.Maybe

import GraphMatch
import GPSyntax
import LabelMatch
import GraphMatch
import Graph
import ExAr
import GPCondition

-- Calls matchGraphs to get the list of morphisms, removes those that do not
-- pass the dangling condition and returns the list of HostGraphs where 
-- each HostGraph is obtained from applying the rule with respect to each morphism.

applyRule :: HostGraph -> Rule -> [HostGraph]
applyRule h r@(Rule _ _ (lhs, rhs) i cond) =
        [ transform m r h | m <- filter (danglingCondition h r) $ matchGraphs h lhs]

-- criticalHostEdges is the set of host edges incident to nodes deleted by the rule.
-- preservedHostEdges is the set of host edges not deleted by the rule.
-- The dangling condition is violated if these sets overlap.

danglingCondition :: HostGraph -> Rule -> GraphMorphism -> Bool
danglingCondition h r m = null $ criticalHostEdges `intersect` preservedHostEdges
    where criticalHostEdges = concatMap (incidentEdges h) deletedHostNodes
          preservedHostEdges = allEdges h \\ map snd ems
          deletedHostNodes = [ hnid | lnid <- deletedLhsNodes, let hnid = lookup' lnid nms ]
          deletedLhsNodes = allNodes lhs \\ map fst intr
          (Rule _ _ (lhs,rhs) intr _) = r
          GM _ nms ems = m


-- Creates the new host graph from the old host graph H in three steps:
-- (1) Remove the images of all LHS edges and any deleted nodes from H. 
--     The deleted nodes are found by taking the set difference of 
--     'allNodes lhs' and the interface. 
-- (2) Add new nodes to the graph and relabel existing nodes. This is done
--     with a call to the function addNodesToHost which outputs the modified 
--     host graph and a mapping from RHS NodeIds to Host NodeIds to
--     facilitate the next step.
-- (3) Add the edges to the graph. 

transform :: GraphMorphism -> Rule -> HostGraph -> HostGraph
transform m r h = addEdgesToHost m rhs rhsToHostMap addedNodesGraph
    where 
      removedItemsGraph = rmNodeList (rmEdgeList h deletedHostEdges) deletedHostNodes                 
      (addedNodesGraph, rhsToHostMap) = addNodesToHost m r removedItemsGraph
      deletedHostEdges  = [ heid | leid <- allEdges lhs, let heid = lookup' leid ems ]
      deletedHostNodes  = [ hnid | lnid <- deletedLhsNodes, let hnid = lookup' lnid nms ]
      deletedLhsNodes   = allNodes lhs \\ map fst intr
      Rule _ _ (lhs, rhs) intr _ = r
      GM env nms ems                = m


-- We generate a mapping from all NodeIds in the RHS, including freshly 
-- created nodes, to their NodeIDs in the host graph. This mapping is used 
-- to add the edges between the correct nodes.
--
-- The mapping is created by appending two lists: the list generated from
-- the interface and the graph morphism, and the zip of the new RHS nodes
-- with the new host nodes (obtained from the call to newNodeList).

addNodesToHost :: GraphMorphism -> Rule -> HostGraph -> (HostGraph, NodeMatches)
addNodesToHost m r h = (h'', relabelledHostNodes ++ newHostNodes)
    where
        h'' = foldr relabelNode h' relabelledHostNodes 
        (h', insertedHostNodes) = newNodeList h $ map (nodeEval m h rhs) ruleNodes

        relabelNode :: (RuleNodeId, HostNodeId) -> HostGraph -> HostGraph
        relabelNode (rid, hid) h = nReLabel h hid newLabel
            where newLabel = nodeEval m h rhs $ nLabel rhs rid 
          
        relabelledHostNodes = [ (ri, hi) | (li, ri) <- intr, let hi = lookup' li nms ]
        newHostNodes = zip insertedRhsNodes insertedHostNodes
        ruleNodes = map (nLabel rhs) insertedRhsNodes
        insertedRhsNodes  = allNodes rhs \\ map snd intr
        GM env nms ems = m
        Rule _ _ (_, rhs) intr _ = r


nodeEval :: GraphMorphism -> HostGraph -> RuleGraph -> RuleNode -> HostNode
nodeEval m h r rn@(RuleNode name isRoot label) = HostNode name isRoot $ labelEval m h r label

-- Adds all RHS edges to the graph. The NodeMatches argument stores the mappings of
-- all RHS NodeIds to host graph NodeIds. This is used to create the correct
-- arguments for the call to newEdgeList.

addEdgesToHost :: GraphMorphism -> RuleGraph -> NodeMatches -> HostGraph -> HostGraph
addEdgesToHost m rhs nms h = fst $ newEdgeList h newEdges
    where
        newEdges = [ (hsrc, htgt, hlabel) | re <- allEdges rhs, 
                      let hsrc = lookup' (source rhs re) nms
                          htgt = lookup' (target rhs re) nms
                          hlabel = labelEval m h rhs (eLabel rhs re) ]

