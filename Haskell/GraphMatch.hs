module GraphMatch where

import Data.List
import Data.Maybe
import Graph
import List
import Mapping
import LabelMatch
import GPSyntax

type RuleNodeId = NodeId 
type HostNodeId = NodeId
type RuleEdgeId = EdgeId
type HostEdgeId = EdgeId

type NodeMatches = Mapping RuleNodeId HostNodeId
type EdgeMatches = Mapping RuleEdgeId HostEdgeId

data GraphMorphism = GM Environment NodeMatches EdgeMatches deriving (Show) 
data NodeMorphism = NM Environment NodeMatches deriving (Show)

-- Graph-matching proceeds by finding a candidate node-matches,
-- and for each of these the associated edge-matches, if any.
matchGraphs :: HostGraph -> RuleGraph -> [GraphMorphism]
matchGraphs h r = [gm | nm <- matchGraphNodes h r, gm <- matchGraphEdges h r nm]

-- Candidate node matches are those for which the rule-node labels
-- match the host-node labels; and if any rule-node is a root
-- the matching host-node must be a root.
-- It must also be possible to merge the results of label-matching
-- across all nodes.
matchGraphNodes :: HostGraph -> RuleGraph -> [NodeMorphism]
matchGraphNodes h r =
   [ NM env (zip ruleNodes hns) | hnenvs <- choices hostNodeMatches,
                                  let (hns,envs) = unzip hnenvs,
                                  isSet hns,
                                  Just env <- [mergeMappings envs] ]
   where
   ruleNodes            =  allNodes r
   ruleNodesWithLabels  =  [   (rn,lab) | rn <- ruleNodes,
                                          let RuleNode _ _ lab = nLabel r rn ]
   hostNodesWithLabels  =  [   (hn,lab) | hn <- allNodes h,
                                          let HostNode _ _ lab = nLabel h hn ]
   hostNodeMatches      =  [ [ (hn,env) | (hn,hlab) <- hostNodesWithLabels,
                                          rootCompatible h r (hn,rn),
                                          Just env <- [doLabelsMatch hlab rlab] ]
                           | (rn,rlab) <- ruleNodesWithLabels ]

rootCompatible :: HostGraph -> RuleGraph -> (HostNodeId, RuleNodeId) -> Bool
rootCompatible h r (hn, rn) = not (isRootR r rn) || isRootH h hn

-- For each edge in the RuleGraph, we determine the required source and
-- target for a corresponding edge in the HostGraph using the node morphism.
-- Candidate edges can then be found using joiningEdges.  Accepted candidates
-- are those for which there is a consistent label-matching.
matchGraphEdges :: HostGraph -> RuleGraph -> NodeMorphism -> [GraphMorphism]
matchGraphEdges h r (NM env nodeMatches) =
   [ GM labelEnv nodeMatches edgeMatch
   | edgeMatch <- [zip ruleEdges hes | hes <- choices hostEdges, isSet hes],
     Just labelEnv <- [foldr labelMatch (Just env) edgeMatch] ]
   where 
   ruleEdges = allEdges r
   hostEdges = [ joiningEdges h src tgt ++
                 if isBidirectional r reid then joiningEdges h tgt src
                 else []
               | reid <- ruleEdges,
                 let src = definiteLookup (source r reid) nodeMatches,
                 let tgt = definiteLookup (target r reid) nodeMatches ]
   labelMatch :: (RuleEdgeId, HostEdgeId) -> Maybe Environment -> Maybe Environment
   labelMatch (re, he) menv = do
      env <- menv
      let RuleEdge _ _ rlabel = eLabel r re
      mapping <- doLabelsMatch (eLabel h he) rlabel
      mergeMapping mapping env

