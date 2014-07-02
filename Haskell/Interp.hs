module Interp where

import Data.List
import Data.Maybe

import GraphMatch
import GPSyntax
import LabelMatch
import GraphMatch
import Graph
import ExAr
import GPCondition

-- type Subst a b = [(a, b)]
-- type NodeMatches = Subst RuleNodeId HostNodeId
-- type EdgeMatches = Subst RuleEdgeId HostEdgeId
-- data GraphMorphism = GM Environment NodeMatches EdgeMatches

--notImplemented = error "Not implemented"

-- getNodeLabelsForMorphism :: HostGraph -> RuleGraph -> GraphMorphism -> [ (HostLabel, RuleLabel) ]
-- getNodeLabelsForMorphism h r m = notImplemented
{-
getNodesForMorphism :: HostGraph -> RuleGraph -> GraphMorphism -> [ ( HostNode, RuleNode ) ]
getNodesForMorphism h r m = notImplemented

getEdgesForMorphism :: HostGraph -> RuleGraph -> GraphMorphism -> [ ( HostEdge, RuleEdge ) ]
getEdgesForMorphism h r m = notImplemented



checkAtomsFor :: HostGraph -> RuleGraph -> GraphMorphism -> Bool
checkAtomsFor h g m = notImplemented
-}




-- Old Code
-- Filter out any graph morphisms for which RuleGraph conditions are not met
{-
checkAtoms :: HostGraph -> RuleGraph -> [GraphMorphism] -> [GraphMorphism]
checkAtoms h r ms = ms'
    where
        ms' = filter ( checkAtomsFor h r ) ms




substituteNodes :: GraphMorphism -> RuleGraph -> HostGraph -> (HostNodeId, RuleNodeId) -> HostGraph
substituteNodes m r h (hid, rid)  = nReLabel h hid hn'
    where
        hn' = nodeEval m h r $ fromJust $ maybeNLabel r rid 

substituteEdges :: GraphMorphism -> RuleGraph -> HostGraph -> (HostEdgeId, RuleEdgeId) -> HostGraph
substituteEdges m r h (hid, rid) = eReLabel h hid he'
    where
        he' = edgeEval m h r $ fromJust $ maybeELabel r rid

applyMorphism :: RuleGraph -> HostGraph -> GraphMorphism -> HostGraph
applyMorphism r h m@(GM env nms ems) = h''
    where
        h'  = foldl (substituteNodes m r) h nms
        h'' = foldl (substituteEdges m r) h' ems
        -- TODO: add and delete nodes
-}

nodeEval :: GraphMorphism -> HostGraph -> RuleGraph -> RuleNode -> HostNode
nodeEval m h r rn@(RuleNode name isRoot label) = HostNode name isRoot $ labelEval m h r label

-- Not necessary: edges in the host graph are represented only by their label, hence
-- labelEval can be used to evaluate edges.
edgeEval :: GraphMorphism -> HostGraph -> RuleGraph -> RuleEdge -> HostEdge
edgeEval m h r re@(RuleEdge bidi src tgt label) = HostEdge src tgt $ labelEval m h r label


makeRhsEdgeSubst :: GraphMorphism -> Rule -> NodeMatches -> HostGraph -> (HostGraph, EdgeMatches)
makeRhsEdgeSubst m r s h = (h', ems')
    where
        ems' = zip res insertedHostEdges
        res = allEdges rhs
        (h', insertedHostEdges) = newEdgeList h [
            (source rhs re, target rhs re, labelEval m h rhs (eLabel rhs re))
            | re <- res ]
        Rule _ _ (_, rhs) _ _ _ = r

makeRhsNodeSubst :: GraphMorphism -> Rule -> HostGraph -> (HostGraph, NodeMatches)
makeRhsNodeSubst m r h = (h', nms')
    where
        nms' = [ (ri, hi) | (li, ri) <- intr,
                        let Just hi = lookup li nms ]
               ++ zip insertedRhsNodes insertedHostNodes
        (h', insertedHostNodes) = newNodeList h $ map (nodeEval m h rhs) ruleNodes
        -- (h', insertedHostNodes) = newNodeList (length insertedRhsNodes) h
        ruleNodes = map (nLabel rhs) insertedRhsNodes
        insertedRhsNodes  = allNodes rhs \\ map snd intr
        GM env nms ems = m
        Rule _ _ (_, rhs) intr _ _ = r

-- data Rule = Rule RuleName [Variable] (RuleGraph, RuleGraph) 
            -- Interface Condition String

-- no node or edge labels yet!
transform :: GraphMorphism -> Rule -> HostGraph -> Maybe HostGraph
transform m r h = do
    -- we're in the Maybe monad -- gives us "free" handling
    -- of the dangling condition. Yay.
    h' <- rmIsolatedNodeList (rmEdgeList h deletedHostEdges) deletedHostNodes                 
    let (h'', s) = makeRhsNodeSubst m r h'
    let (h3, s') = makeRhsEdgeSubst m r s h''
    return h3
    where
        deletedHostEdges  = [ heid | leid <- allEdges lhs,
                                     let Just heid = lookup leid ems ]
        deletedHostNodes  = [ hnid | lnid <- deletedLhsNodes,
                                     let Just hnid = lookup lnid nms ]
        deletedLhsNodes   = allNodes lhs \\ map fst intr
        Rule _ _ (lhs, rhs) intr _ _  = r
        GM env nms ems                = m

applyRule :: HostGraph -> Rule -> [HostGraph]
applyRule h r@(Rule _ _ (lhs, rhs) i cond _) =
        [ h' | m <- matchGraphs h lhs, let Just h' = transform m r h ]

