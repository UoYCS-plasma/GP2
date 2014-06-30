module Interp where

import Data.Maybe

import GraphMatch
import GPSyntax
import LabelMatch
import GraphMatch
import Graph
import GPCondition

-- type Subst a b = [(a, b)]
-- type NodeMatches = Subst RuleNodeId HostNodeId
-- type EdgeMatches = Subst RuleEdgeId HostEdgeId
-- data GraphMorphism = GM Environment NodeMatches EdgeMatches

notImplemented = error "Not implemented"

-- getNodeLabelsForMorphism :: HostGraph -> RuleGraph -> GraphMorphism -> [ (HostLabel, RuleLabel) ]
-- getNodeLabelsForMorphism h r m = notImplemented

getNodesForMorphism :: HostGraph -> RuleGraph -> GraphMorphism -> [ ( HostNode, RuleNode ) ]
getNodesForMorphism h r m = notImplemented

getEdgesForMorphism :: HostGraph -> RuleGraph -> GraphMorphism -> [ ( HostEdge, RuleEdge ) ]
getEdgesForMorphism h r m = notImplemented



checkAtomsFor :: HostGraph -> RuleGraph -> GraphMorphism -> Bool
checkAtomsFor h g m = notImplemented





-- Filter out any graph morphisms for which RuleGraph conditions are not met
checkAtoms :: HostGraph -> RuleGraph -> [GraphMorphism] -> [GraphMorphism]
checkAtoms h r ms = ms'
    where
        ms' = filter ( checkAtomsFor h r ) ms




ruleNodeToHostNode :: Environment -> RuleNode -> HostNode
ruleNodeToHostNode e rn = notImplemented

ruleEdgeToHostEdge :: Environment -> RuleLabel -> HostLabel
ruleEdgeToHostEdge e rn = notImplemented

substituteNodes :: RuleGraph -> HostGraph -> GraphMorphism -> HostGraph
substituteNodes r h m@(GM _ nms ems) = nReLabel h hid hn'
    where
        hn' = labelEval m r $ fromJust $ nLabel r rid 

substituteEdges :: Environment -> RuleGraph -> HostGraph -> (HostEdgeId, RuleEdgeId) -> HostGraph
substituteEdges env r h (hid, rid) = eReLabel h hid he'
    where
        he' = ruleEdgeToHostEdge env $ fromJust $ eLabel r rid

applyMorphism :: RuleGraph -> HostGraph -> GraphMorphism -> HostGraph
applyMorphism r h (GM env nms ems) = h''
    where
        h'  = foldl (substituteNodes env r) h nms
        h'' = foldl (substituteEdges env r) h' ems


