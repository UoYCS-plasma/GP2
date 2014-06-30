module Interp where

import Data.Maybe

import GraphMatch
import GPSyntax
import LabelMatch
import GraphMatch
import Graph
--import GPCondition

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
ruleNodeToHostNode e r = notImplemented

ruleEdgeToHostEdge :: Environment -> RuleEdge -> HostEdge
ruleEdgeToHostEdge e r = notImplemented

substituteNodes :: RuleGraph -> HostGraph -> Environment -> RuleNodeId -> HostNodeId -> HostGraph
substituteNodes r h env (hid, rid) = nReLabel h hid hn'
    where
        hn' = ruleNodeToHostNode env $ fromJust $ nLabel h hid 

substituteEdges :: RuleGraph -> HostGraph -> Environment -> RuleEdgeId -> HostEdgeId -> HostGraph
substituteEdges r h env (hid, rid) = eReLabel h hid he'
    where
        he' = ruleEdgeToHostEdge env $ fromJust $ eLabel h hid

applyMorphism :: RuleGraph -> HostGraph -> GraphMorphism -> HostGraph
applyMorphism r h (GM env nms ems) = notImplemented
    where
        nodes' = map ((substituteNodes r h env) . fromJust . (nLabel r) . fst) nms
        edges' = map ((substituteEdges r h env) . fromJust . (eLabel r) . fst) ems


