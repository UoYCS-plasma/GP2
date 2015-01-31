module GraphMatch where

import Data.List
-- import Data.Maybe
import Graph
import List
import Mapping
import LabelMatch
import GPSyntax

-- Rule nodes are mapped to host nodes, and rule edges to host edges.
type NodeMatches = Mapping NodeKey NodeKey
type EdgeMatches = Mapping EdgeKey EdgeKey

data GraphMorphism = GM Environment NodeMatches EdgeMatches deriving (Show) 
data NodeMorphism  = NM Environment NodeMatches deriving (Show)

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
   [ NM env (zip rns hns) | hnenvs <- choices hostNodeMatches,
                            let (hns,envs) = unzip hnenvs,
                            isSet hns,
                            Just env <- [mergeMappings envs] ]
   where
   (rns,ruleNodes)      =  unzip $ allNodes r
   hostNodeMatches      =  [ [ (hnk,env)
                             | (hnk,HostNode _ hroot hlab) <- allNodes h,
                               not rroot || hroot,
                               Just env <- [doLabelsMatch hlab rlab] ]
                           | RuleNode _ rroot rlab <- ruleNodes ]

-- For each edge in the RuleGraph, we determine the required source and
-- target for a corresponding edge in the HostGraph using the node morphism.
-- Candidate edges can then be found using joiningEdges.  Accepted candidates
-- are those for which there is a consistent label-matching.
matchGraphEdges :: HostGraph -> RuleGraph -> NodeMorphism -> [GraphMorphism]
matchGraphEdges h r (NM env nmap) =
   [ GM env' nmap (zip res hes) | heenvs <- choices hostEdgeMatches,
                                  let (hes,envs) = unzip heenvs,
                                  isSet [edgeNumber he | he <- hes],
                                  Just env' <- [mergeMappings $ env : envs] ]
   where 
   ruleEdges        =  allEdges r
   (res,_)          =  unzip ruleEdges
   hostEdgeMatches  =  [ [ (hek,env)
                         | (hek,hlab) <-
                             joiningEdges h src tgt ++
                             if bidir then joiningEdges h tgt src else [],
                             Just env <- [doLabelsMatch hlab rlab] ]
                       | (rek,RuleEdge _ bidir rlab) <- ruleEdges,
                         let src = definiteLookup (source rek) nmap,
                         let tgt = definiteLookup (target rek) nmap ]

