module ApplyRule where

import Data.List
import Data.Maybe
import GPSyntax
import LabelMatch
import GraphMatch
import Graph
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
    let deletedNodes = [ definiteLookup nk nms
                       | nk <- allNodeKeys lhs, nk `notElem` dom ni],
    danglingCondition h ems deletedNodes,
    conditionEval (m, h, lhs) cond ]

-- The dangling condition restricts valid matches.  There must not be any
-- host edges that would be preserved though they are incident to nodes that
-- would be deleted.
danglingCondition :: HostGraph -> EdgeMatches -> [NodeKey] -> Bool
danglingCondition h ems delns = 
  null [ek | hn <- delns, (ek,_) <- incidentEdges h hn, ek `notElem` rng ems]

-- Creates the new host graph from the old host graph H in three steps:
-- (1) Remove the images of all LHS edges and any deleted nodes from H.
--     The deleted nodes list is computed by the caller (applyRule) and passed
--     as the fourth argument to transform.
-- (2) Add new nodes to the graph and relabel existing nodes. This is done
--     with a call to the function addNodesToHost which outputs the modified 
--     host graph and a mapping from RHS NodeIds to Host NodeIds to
--     facilitate the next step.
-- (3) Add the edges to the graph. 
transform :: GraphMorphism -> Rule -> HostGraph -> [NodeKey] -> HostGraph
transform m@(GM _ _ ems) r@(Rule _ _ (lhs, rhs) ni ei _) h hns = 
   addEdgesToHost m rhs ei rhsToHostMap addedNodesGraph   
   where 
   (addedNodesGraph, rhsToHostMap) = addNodesToHost m lhs rhs ni removedItemsGraph
   removedItemsGraph = rmNodeList (rmEdgeList h deletedHostEdges) hns                 
   deletedHostEdges  = [ definiteLookup leid ems | leid <- allEdgeKeys lhs \\ dom ei ]

-- We generate a mapping from all NodeIds in the RHS, including freshly 
-- created nodes, to their NodeIDs in the host graph. This mapping is used 
-- to add the edges between the correct nodes.
--
-- The mapping is created by appending two lists: the list generated from
-- the interface and the graph morphism, and the zip of the new RHS nodes
-- with the new host nodes (obtained from the call to newNodeList).
addNodesToHost :: GraphMorphism -> RuleGraph -> RuleGraph -> NodeInterface -> HostGraph -> (HostGraph, NodeMatches)
addNodesToHost m@(GM _ nms _) lhs rhs ni h = 
    (h'', relabelMapping ++ zip insertedRhsNodes insertedHostNodes)
    where
    h''                     = foldr (relabelNode m lhs rhs) h' relabelMapping 
    (h', insertedHostNodes) = newNodeList h [ nodeEval (m, h, rhs) (nLabel lhs n) (nLabel rhs n) Nothing
                                            | n <- insertedRhsNodes ]
    relabelMapping          = [ (ri, definiteLookup li nms) | (li, ri) <- ni]
    insertedRhsNodes        = allNodeKeys rhs \\ rng ni

relabelNode :: GraphMorphism -> RuleGraph -> RuleGraph -> (NodeKey, NodeKey) -> HostGraph -> HostGraph
relabelNode m lhs rhs (rnk, hnk) h = nReLabel h hnk
                               $ nodeEval (m, h, rhs)
                                    (nLabel lhs rnk)
                                    (nLabel rhs rnk)
                                    (Just $ nLabel h hnk)

-- TODO: incorrect root handling! In order to correctly set the root flag we
-- need to know the root flag of _both_ the LHS and the RHS of the rule, as a
-- non-root LHS can match a root host node. Using only the RHS root flag like
-- this causes roots to be wrongly set and unset.

nodeEval :: EvalContext -> RuleNode -> RuleNode -> Maybe HostNode -> HostNode
nodeEval ec lrn@(RuleNode _ isLRoot _) rrn@(RuleNode name isRRoot label) hn = HostNode name rootStatus $ fixColour hn $ labelEval ec label
    where rootStatus = case (isLRoot, isRRoot) of
                            (True, True)   -> True
                            (True, False)  -> False
                            (False, True)  -> True
                            (False, False) -> curRootStatus hn

          curRootStatus :: Maybe HostNode -> Bool
          curRootStatus Nothing                 = False
          curRootStatus (Just (HostNode _ r _)) = r

          fixColour :: Maybe HostNode -> HostLabel -> HostLabel
          fixColour (Just n) (HostLabel l Any) = (HostLabel l $ hostColour n)
          fixColour Nothing  (HostLabel l Any) = error "Tried to create a Any-colour node!" 
          fixColour _ l = l
          hostColour (HostNode _ _ (HostLabel _ c ) ) = c


-- Adds all RHS edges to the graph. The NodeMatches argument stores the mappings of
-- all RHS NodeIds to host graph NodeIds.
addEdgesToHost :: GraphMorphism -> RuleGraph -> EdgeInterface -> NodeMatches -> HostGraph -> HostGraph
addEdgesToHost m@(GM _ _ ems) rhs ei nms h = 
    foldr (relabelEdge m rhs)  h' [(ri, definiteLookup li ems) | (li, ri) <- ei]
    where
    (h',_) = newEdgeList h [ (hsrc, htgt, labelEval (m, h, rhs) label)
                           | (ek,RuleEdge _ _ label) <- allEdges rhs,
                             ek `notElem` rng ei, 
                             let hsrc = definiteLookup (source ek) nms,
                             let htgt = definiteLookup (target ek) nms ]

relabelEdge :: GraphMorphism -> RuleGraph -> (EdgeKey,EdgeKey) -> HostGraph -> HostGraph
relabelEdge m rhs (rek, hek) h = eReLabel h hek $ edgeEval (m, h, rhs) $ eLabel rhs rek 

edgeEval :: EvalContext -> RuleEdge -> HostLabel
edgeEval ec re@(RuleEdge _ _ label) = labelEval ec label

