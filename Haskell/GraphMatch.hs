module GraphMatch where

import Prelude hiding (lookup)
import Data.List
import Data.Maybe
import Control.Monad (guard)


import ExAr
import Graph
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

-- TODO(?): Test if the rule graphs are valid graphs.
matchGraphs :: HostGraph -> RuleGraph -> [GraphMorphism]
matchGraphs h r = concatMap (matchGraphEdges h r) $ matchGraphNodes h r


-- Outputs all valid (w.r.t labels) injective morphisms (node morphisms)
-- from the nodes of LHS to the nodes of the host graph.
-- We generate all candidate sets of nodes in the host graph. For an LHS with
-- k nodes, this is the set of size-k subsets of the node set of the host graph,
-- including permutations.
-- These subsets are zipped with the node set of the LHS to create sets of 
-- candidate node morphisms. Morphisms that map LHS root nodes to non-root nodes
-- in the host graph are pruned from the set. The remaining node morphisms 
-- are tested with doNodesMatch, called by labelMatch.
-- labelMatch returns a Maybe Environment. We use the maybe function to lift
-- each Maybe Environment into a Maybe NodeMorphism. FInally we call catMaybes
-- on the resulting list in order to filter out the Nothings.

matchGraphNodes :: HostGraph -> RuleGraph -> [NodeMorphism]
matchGraphNodes h r =
    catMaybes [ nm | nodeSet <- nodeSets, 
          let nodeMatches = filter (checkRootNode h r) $ zip rns nodeSet
              maybeEnv = foldr labelMatch (Just []) nodeMatches
              nm = maybe Nothing (\env -> Just (NM env nodeMatches) ) maybeEnv ]
    where
        rns = allNodes r
        hns = allNodes h
        nodeSets = permutedSizedSubsets (length rns) hns

        -- Match node labels, further building the environment of variable-value mappings.
        labelMatch :: (RuleNodeId, HostNodeId) -> Maybe Environment -> Maybe Environment
        labelMatch (rn, hn) menv = do
                  env <- menv
                  mapping <- doNodesMatch h r hn rn
                  mergeMapping mapping env

checkRootNode :: HostGraph -> RuleGraph -> (RuleNodeId, HostNodeId) -> Bool
checkRootNode h r (rid, hid) = case (rb, hb) of
        -- Concerning root nodes, a node mapping is invalid only in the case
        -- where a LHS root node is mapped to a non-root host graph node.
        (True, False) -> False
        _ -> True
        where
            RuleNode _ rb _ = nLabel r rid
            HostNode _ hb _ = nLabel h hid 
                
doNodesMatch :: HostGraph -> RuleGraph -> HostNodeId -> RuleNodeId -> Maybe Environment
doNodesMatch h r hid rid = 
    let hnode = (maybeNLabel h hid)
        rnode = (maybeNLabel r rid) in
    case (hnode, rnode) of 
         (Nothing, _) -> Nothing
         (_, Nothing) -> Nothing
         (Just (HostNode _ _ hlabel), Just (RuleNode _ _ rlabel)) 
                      -> doLabelsMatch hlabel rlabel


-- For each edge in the RuleGraph, we generate a pair of its source and target
-- node (ruleEndPoints).
-- Each of these pairs is transformed to the source and target node in the host
-- graph using the NodeMorphism (hostEndPoints).
-- joiningEdges is called on each hostEndPoint to generate all the candidate
-- edges for each RuleEdge. A RuleEdge may have more than one candidate
-- HostEdge, so a list of lists is created (hostEdges).
-- hostEdges is used to generate edgeMatches, which are passed to the list
-- comprehension in the same way as in the node matcher.

matchGraphEdges :: HostGraph -> RuleGraph -> NodeMorphism -> [GraphMorphism]
matchGraphEdges h r (NM env nodeMatches) =
   catMaybes [ gm | edgeMatch <- edgeMatches,
                    let maybeEnv = foldr labelMatch (Just env) edgeMatch
                        gm = maybe Nothing (\env -> Just (GM env nodeMatches edgeMatch) ) maybeEnv ]
   where 
     ruleEdges = allEdges r
     ruleEndPoints = map (\e -> (source r e, target r e)) ruleEdges
     hostEndPoints = mapMaybe ruleEndsToHostEnds ruleEndPoints
     hostEdges = map (\(src, tgt) -> [eid | eid <- joiningEdges h src tgt]) hostEndPoints

     -- The ith RuleEdgeId eid in ruleEdges corresponds to the ith [HostEdgeId] heids in 
     -- hostEdges. eid needs to be paired up with each item in heids e.g.
     -- [E 1, E 2] [[E 1, E 2],[E 1, E 2, E 3]] -> 
     -- [(E 1, E 1), (E 1, E 2), (E 2, E 1), (E 2, E 2), (E 2, E 3)]
     edgeMatches = zipWith (\re hes -> zip (repeat re) hes) ruleEdges hostEdges

     -- The source and target node aren't guaranteed to exist in the node morphism,
     -- hence we have to use a standard lookup and pattern match on the pair of 
     -- Maybe NodeIds.
     ruleEndsToHostEnds :: (RuleNodeId, RuleNodeId) -> Maybe (HostNodeId, HostNodeId)
     ruleEndsToHostEnds (src, tgt) = case (lookup src nodeMatches, lookup tgt nodeMatches) of
        (Just s, Just t) -> Just (s, t)
        _                -> Nothing

     labelMatch :: (RuleEdgeId, HostEdgeId) -> Maybe Environment -> Maybe Environment
     labelMatch (re, he) menv = do
               env <- menv
               mapping <- doEdgesMatch h r he re
               mergeMapping mapping env
            
doEdgesMatch :: HostGraph -> RuleGraph -> HostEdgeId -> RuleEdgeId -> Maybe Environment
doEdgesMatch h r hid rid = 
   let mhlabel = (maybeELabel h hid)
       mrlabel = (maybeELabel r rid) in
   case (mhlabel, mrlabel) of 
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just hlabel, Just rlabel) -> doLabelsMatch hlabel rlabel
        

