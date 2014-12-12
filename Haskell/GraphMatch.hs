module GraphMatch where

import Data.List
import Data.Maybe

import ExAr
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

-- matchGraphEdges h r generates a list of GraphMorphisms from a single
-- NodeMorphism. In order to generate the complete list of GraphMorphisms,
-- we concatMap matchGraphEdges h r over the NodeMorphism list obtained
-- from the call to matchGraphNodes.
-- In the case that the rule graph is the empty graph, we immediately
-- return the empty morphism.
matchGraphs :: HostGraph -> RuleGraph -> [GraphMorphism]
matchGraphs h r = [gm | nm <- matchGraphNodes h r, gm <- matchGraphEdges h r nm]

-- Outputs all valid (w.r.t labels) injective morphisms (node morphisms)
-- from the nodes of LHS to the nodes of the host graph.
--
-- We generate all candidate sets of nodes in the host graph. For an LHS with
-- k nodes, this is the set of size-k subsets of the node set of the host graph,
-- including permutations.
--
-- These subsets are zipped with the node set of the LHS to create the complete
-- set of candidate node morphisms. Accepted candidates are those for which
-- (1) rule nodes are mapped to compatible host nodes that agree wrt
-- rootedness, in-degree, out-degree and colour, and
-- (2) a consistent label-matching can be found.
matchGraphNodes :: HostGraph -> RuleGraph -> [NodeMorphism]
matchGraphNodes h r =
   [ NM labelEnv nodeMatches
   | nodeMatches <- [ nm | hns <- permutedSizedSubsets (length rns) (allNodes h),
                           let nm = zip rns hns,
                           all (compatibleNodes h r) nm ],
     Just labelEnv <- [foldr labelMatch (Just []) nodeMatches] ]
   where
   rns = allNodes r
   labelMatch :: (RuleNodeId, HostNodeId) -> Maybe Environment -> Maybe Environment
   labelMatch (rn, hn) menv = do
      env <- menv
      (RuleNode _ _ rlab) <- maybeNLabel r rn
      (HostNode _ _ hlab) <- maybeNLabel h hn
      mapping <- doLabelsMatch hlab rlab
      mergeMapping mapping env

compatibleNodes :: HostGraph -> RuleGraph -> (RuleNodeId, HostNodeId) -> Bool
compatibleNodes h r (rn, hn) = (not (isRootR r rn) || isRootH h hn) &&
                               indegree r rn <= indegree h hn &&  
                               outdegree r rn <= outdegree h hn &&
                               colourMatch (colourR r rn) (colourH h hn)

-- For each edge in the RuleGraph, we determine the required source and
-- target for a corresponding edge in the HostGraph using the node morphism.
-- Candidate edges can then be found using joiningEdges.  Accepted candidates
-- are those for which there is a consistent label-matching.
matchGraphEdges :: HostGraph -> RuleGraph -> NodeMorphism -> [GraphMorphism]
matchGraphEdges h r (NM env nodeMatches) =
   [ GM labelEnv nodeMatches edgeMatch
   | [] `notElem` hostEdges,
     edgeMatch <- [zip ruleEdges hes | hes <- choices hostEdges, isSet hes],
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
      hlabel <- maybeELabel h he
      RuleEdge _ _ rlabel <- maybeELabel r re
      mapping <- doLabelsMatch hlabel rlabel
      mergeMapping mapping env

