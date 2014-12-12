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

-- The list comprehension collects the host graphs returned by applying the rule
-- with respect to all valid matches from the LHS to the host graph.
-- matchGraphs h lhs generates the set of morphisms which is then filtered using
-- tests for the dangling condition and any conditions attached to the rule.
-- Valid morphisms are used to generate host graphs by applying transform.
applyRule :: HostGraph -> Rule -> [HostGraph]
applyRule h r@(Rule _ _ (lhs, _) ni _ cond) =
        [ transform m r h deletedNodes
        | m@(GM _ nms ems) <- matchGraphs h lhs,
          let deletedNodes = [definiteLookup lnid nms | lnid <- allNodes lhs \\ dom ni],
          danglingCondition h ems deletedNodes,
          conditionEval (m, h, lhs) cond ]

-- The dangling condition restricts valid matches.  There must not be any
-- host edges that would be preserved though they are incident to nodes that
-- would be deleted.
danglingCondition :: HostGraph -> EdgeMatches -> [NodeId] -> Bool
danglingCondition h ems delns = null [e | hn <- delns, e <- incidentEdges h hn \\ rng ems]

-- Creates the new host graph from the old host graph H in three steps:
-- (1) Remove the images of all LHS edges and any deleted nodes from H.
--     The deleted nodes list is computed by the caller (applyRule) and passed
--     as the fourth argument to transform.
-- (2) Add new nodes to the graph and relabel existing nodes. This is done
--     with a call to the function addNodesToHost which outputs the modified 
--     host graph and a mapping from RHS NodeIds to Host NodeIds to
--     facilitate the next step.
-- (3) Add the edges to the graph. 
transform :: GraphMorphism -> Rule -> HostGraph -> [NodeId] -> HostGraph
transform m@(GM _ _ ems) r@(Rule _ _ (lhs, rhs) ni ei _) h hns = 
   addEdgesToHost m rhs ei rhsToHostMap addedNodesGraph   
   where 
   (addedNodesGraph, rhsToHostMap) = addNodesToHost m rhs ni removedItemsGraph
   removedItemsGraph = rmNodeList (rmEdgeList h deletedHostEdges) hns                 
   deletedHostEdges  = [ definiteLookup leid ems | leid <- allEdges lhs \\ dom ei ]

-- We generate a mapping from all NodeIds in the RHS, including freshly 
-- created nodes, to their NodeIDs in the host graph. This mapping is used 
-- to add the edges between the correct nodes.
--
-- The mapping is created by appending two lists: the list generated from
-- the interface and the graph morphism, and the zip of the new RHS nodes
-- with the new host nodes (obtained from the call to newNodeList).
addNodesToHost :: GraphMorphism -> RuleGraph -> NodeInterface -> HostGraph -> (HostGraph, NodeMatches)
addNodesToHost m@(GM _ nms _) rhs ni h = 
    (h'', relabelMapping ++ zip insertedRhsNodes insertedHostNodes)
    where
    h''                     = foldr (relabelNode m rhs) h' relabelMapping 
    (h', insertedHostNodes) = newNodeList h [ nodeEval (m, h, rhs) (nLabel rhs n)
                                            | n <- insertedRhsNodes ]
    relabelMapping          = [ (ri, definiteLookup li nms) | (li, ri) <- ni]
    insertedRhsNodes        = allNodes rhs \\ rng ni

relabelNode :: GraphMorphism -> RuleGraph -> (RuleNodeId, HostNodeId) -> HostGraph -> HostGraph
relabelNode m rhs (rid, hid) h = nReLabel h hid $ nodeEval (m, h, rhs) $ nLabel rhs rid 

nodeEval :: EvalContext -> RuleNode -> HostNode
nodeEval ec (RuleNode name isRoot label) = HostNode name isRoot $ labelEval ec label

-- Adds all RHS edges to the graph. The NodeMatches argument stores the mappings of
-- all RHS NodeIds to host graph NodeIds.
addEdgesToHost :: GraphMorphism -> RuleGraph -> EdgeInterface -> NodeMatches -> HostGraph -> HostGraph
addEdgesToHost m@(GM _ _ ems) rhs ei nms h = 
    foldr (relabelEdge m rhs)  h' [(ri, definiteLookup li ems) | (li, ri) <- ei]
    where
    (h',_) = newEdgeList h [ (hsrc, htgt, labelEval (m, h, rhs) rlabel)
                           | re <- allEdges rhs \\ rng ei, 
                             let hsrc = definiteLookup (source rhs re) nms,
                             let htgt = definiteLookup (target rhs re) nms,
                             let RuleEdge _ _ rlabel = eLabel rhs re ]

relabelEdge :: GraphMorphism -> RuleGraph -> (RuleEdgeId,HostEdgeId) -> HostGraph -> HostGraph
relabelEdge m rhs (rid, hid) h = eReLabel h hid $ edgeEval (m, h, rhs) $ eLabel rhs rid 

edgeEval :: EvalContext -> RuleEdge -> HostLabel
edgeEval ec re@(RuleEdge _ _ label) = labelEval ec label

