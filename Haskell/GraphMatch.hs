module GraphMatch where

import Prelude hiding (lookup)
import Data.List
import Data.Maybe
import Control.Monad (guard)

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
-- set of candidate node morphisms (allNodeMatches). Morphisms that map LHS
-- root nodes to non-root nodes in the host graph are filtered from the set. 
-- The remaining node morphisms are tested with labelMatch.
--
-- labelMatch returns a Maybe Environment. We use the maybe function to lift
-- each Maybe Environment into a Maybe NodeMorphism. We call catMaybes
-- on the resulting list in order to filter out the Nothings.
matchGraphNodes :: HostGraph -> RuleGraph -> [NodeMorphism]
matchGraphNodes h r = -- filter (\m -> case m of NM _ [] -> False ; _ -> True) $
   catMaybes [ nm | nodeMatches <- filter (checkNodes h r) $ allNodeMatches,
                let maybeEnv = foldr labelMatch (Just []) nodeMatches
                    nm = maybe Nothing (\env -> Just (NM env nodeMatches) ) maybeEnv ]
   where
   rns = allNodes r
   hns = allNodes h
   nodeSets = permutedSizedSubsets (length rns) hns
   allNodeMatches = map (zip rns) nodeSets

   -- Match node labels, further building the environment of variable-value mappings.
   labelMatch :: (RuleNodeId, HostNodeId) -> Maybe Environment -> Maybe Environment
   labelMatch (rn, hn) menv = do
      env <- menv
      (RuleNode _ _ rlab) <- maybeNLabel r rn
      (HostNode _ _ hlab) <- maybeNLabel h hn
      mapping <- doLabelsMatch hlab rlab
      mergeMapping mapping env

checkNodes :: HostGraph -> RuleGraph -> [(RuleNodeId, HostNodeId)] -> Bool
checkNodes h r nms = all compatible nms
   where
   compatible (rn, hn) = (not (isRootR r rn) || isRootH h hn) &&
                          indegree r rn <= indegree h hn &&  
                          outdegree r rn <= outdegree h hn &&
                          colourMatch (colourR r rn) (colourH h hn)

-- For each edge in the RuleGraph, we generate a pair of its source and target
-- node (ruleEndPoints).
-- Each of these pairs is transformed to the source and target node in the host
-- graph using the node morphism (hostEndPoints).
-- joiningEdges is called on each hostEndPoint to generate all the candidate
-- edges for each RuleEdge. A RuleEdge may have more than one candidate
-- HostEdge, so a list of lists is created (hostEdges).
-- hostEdges is used to generate edgeMatches, analogous to nodeMatches in 
-- matchGraphNodes, which is passed to the list comprehension in the same way 
-- as in the node matcher.

matchGraphEdges :: HostGraph -> RuleGraph -> NodeMorphism -> [GraphMorphism]
matchGraphEdges h r (NM env nodeMatches) = {-if null (allEdges r) then [GM env nodeMatches []] else -}
   catMaybes [ gm | edgeMatch <- map (zip ruleEdges) (choices hostEdges),
                let maybeEnv = foldr labelMatch (Just env) edgeMatch
                    gm = maybe Nothing (\env -> Just (GM env nodeMatches edgeMatch) ) maybeEnv ]
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
            
     -- The kth edge ID in ruleEdges corresponds to the kth list of host edge
     -- IDs in hostEdges. They are combined into a list containing all possible
     -- mappings from left-edges to host-edges where each mapping is a list of
     -- pairs. This is achieved by the generateMatches function, defined in 
     -- Graph.hs.
     --
     -- An example input is [LE1, LE2] [[HE1, HE2], [HE3, HE4, HE5]] 
     --
     -- This means that left-edge LE1 has two candidate host-edges to which it could 
     -- be mapped (HE1 and HE2), while LE2 has three candidates (HE3, HE4 and HE5).
     -- 
     -- The output is
     -- [[(LE1, HE1), (LE2, HE3)], [(LE1, HE1), (LE2, HE4)], [[(LE1, HE1), (LE2, HE5)],
     --  [(LE1, HE2), (LE2, HE3)], [(LE1, HE2), (LE2, HE4)], [(LE1, HE2), (LE2, HE5)]]
     -- 
     -- Each inner list describes a mapping from the complete set of left-edges to
     -- an appropriate set of host-edges.



