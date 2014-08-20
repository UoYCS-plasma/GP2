module ApplyRule where

import Data.List
import Data.Maybe

import GraphMatch
import GPSyntax
import LabelMatch
import GraphMatch
import Graph
import ExAr
import Mapping
import Evaluate 

-- Calls matchGraphs to get the list of morphisms, removes those that do not
-- pass the dangling condition and returns the list of HostGraphs where 
-- each HostGraph is obtained from applying the rule with respect to each morphism.
applyRule :: HostGraph -> Rule -> [HostGraph]
applyRule h r@(Rule _ _ (lhs, _) _ _ _) =
        [ transform m r h | m <- filter (validMorphism h r) $ matchGraphs h lhs]


validMorphism :: HostGraph -> Rule -> GraphMorphism -> Bool
validMorphism h r@(Rule _ _ (lhs, _) _ _ cond) gm = 
        danglingCondition h r gm && conditionEval cond gm h lhs 

-- criticalHostEdges is the set of host edges incident to nodes deleted by the rule.
-- preservedHostEdges is the set of host edges not deleted by the rule.
-- The dangling condition is violated if these sets overlap.

danglingCondition :: HostGraph -> Rule -> GraphMorphism -> Bool
danglingCondition h r m = null $ criticalHostEdges `intersect` preservedHostEdges
    where criticalHostEdges = concatMap (incidentEdges h) deletedHostNodes
          preservedHostEdges = allEdges h \\ map snd ems
          deletedHostNodes = [ hnid | lnid <- deletedLhsNodes, let hnid = case lookup lnid nms of Just n -> n ]
          deletedLhsNodes = allNodes lhs \\ map fst intr
          (Rule _ _ (lhs,rhs) intr _ _) = r
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
transform m r h = addEdgesToHost m r rhsToHostMap addedNodesGraph
    where 
      removedItemsGraph = rmNodeList (rmEdgeList h deletedHostEdges) deletedHostNodes                 
      (addedNodesGraph, rhsToHostMap) = addNodesToHost m r removedItemsGraph
      deletedHostEdges  = [ heid | leid <- allEdges lhs \\ map fst ei, let heid = definiteLookup leid ems]
      deletedHostNodes  = [ hnid | lnid <- deletedLhsNodes, let hnid = definiteLookup lnid nms]
      deletedLhsNodes   = allNodes lhs \\ map fst ni
      Rule _ _ (lhs, rhs) ni ei _ = r
      GM env nms ems             = m


-- We generate a mapping from all NodeIds in the RHS, including freshly 
-- created nodes, to their NodeIDs in the host graph. This mapping is used 
-- to add the edges between the correct nodes.
--
-- The mapping is created by appending two lists: the list generated from
-- the interface and the graph morphism, and the zip of the new RHS nodes
-- with the new host nodes (obtained from the call to newNodeList).

addNodesToHost :: GraphMorphism -> Rule -> HostGraph -> (HostGraph, NodeMatches)
addNodesToHost m r h = (h'', relabelledNodes ++ newHostNodes)
    where
        h'' = foldr relabelNode h' relabelledNodes 
        (h', insertedHostNodes) = newNodeList h $ map (nodeEval m h rhs) ruleNodes

        relabelNode :: (RuleNodeId, HostNodeId) -> HostGraph -> HostGraph
        relabelNode (rid, hid) h = nReLabel h hid newLabel
            where newLabel = nodeEval m h rhs $ nLabel rhs rid 

        nodeEval :: GraphMorphism -> HostGraph -> RuleGraph -> RuleNode -> HostNode
        nodeEval m h r (RuleNode name isRoot label) = HostNode name isRoot $ labelEval m h r label

        relabelledNodes = [ (ri, hi) | (li, ri) <- nintr, let hi = definiteLookup li nms]
        newHostNodes = zip insertedRhsNodes insertedHostNodes
        ruleNodes = map (nLabel rhs) insertedRhsNodes
        insertedRhsNodes  = allNodes rhs \\ map snd nintr
        GM env nms ems = m
        Rule _ _ (_, rhs) nintr _ _ = r


-- Adds all RHS edges to the graph. The NodeMatches argument stores the mappings of
-- all RHS NodeIds to host graph NodeIds. This is used to create the correct
-- arguments for the call to newEdgeList.

addEdgesToHost :: GraphMorphism -> Rule -> NodeMatches -> HostGraph -> HostGraph
addEdgesToHost m@(GM _ _ ems) r nms h = foldr relabelEdge h' relabelledEdges
    where
        h' = fst $ newEdgeList h newEdges
        newEdges = [ (hsrc, htgt, hlabel) | re <- allEdges rhs \\ map snd eintr, 
                      let hsrc = case lookup (source rhs re) nms of Just n -> n
                          htgt = case lookup (target rhs re) nms of Just n -> n
                          hlabel = labelEval m h rhs rlabel 
                          RuleEdge _ _ rlabel = eLabel rhs re]

        relabelEdge (rid, hid) h = eReLabel h hid newLabel
            where newLabel = edgeEval m h rhs $ eLabel rhs rid 

        edgeEval :: GraphMorphism -> HostGraph -> RuleGraph -> RuleEdge -> HostLabel
        edgeEval m h r re@(RuleEdge _ _ label) = labelEval m h r label

        relabelledEdges = [ (ri, hi) | (li, ri) <- eintr, let hi = definiteLookup li ems]
        Rule _ _ (_, rhs) _ eintr _ = r
