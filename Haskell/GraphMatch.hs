module GraphMatch where

import Prelude hiding (lookup)
import Data.List
import Data.Maybe
import Control.Monad (guard)
import LabelMatch

import ExAr
-- import GPGraph
import Graph
import GPSyntax

type RuleNodeId = NodeId 
type HostNodeId = NodeId
type RuleEdgeId = EdgeId
type HostEdgeId = EdgeId

-- type Environment = Subst ID [HostAtom]

type NodeMatches = Subst RuleNodeId HostNodeId
type EdgeMatches = Subst RuleEdgeId HostEdgeId


data GraphMorphism = GM Environment NodeMatches EdgeMatches deriving (Show) 

data NodeMorphism = NM Environment NodeMatches deriving (Show)


permutedSizedSubsets :: Int -> [a] -> [[a]]
permutedSizedSubsets k xs = concatMap perms $ sublistsOf k xs

sublistsOf :: Int -> [a] -> [[a]]
sublistsOf 0 _        = [[]]
sublistsOf _ []       = []
sublistsOf n (x:xs)   = map (x:) (sublistsOf (n-1) xs) ++ sublistsOf n xs

perms :: [a] -> [[a]]
perms []      =  [[]]
perms xs      =  [x:p | (x,xs') <- picks xs, p <- perms xs']

picks :: [a] -> [(a,[a])]
picks []      =  []
picks (x:xs)  =  (x,xs) : [(x',x:xs') | (x',xs') <- picks xs]



lookup' :: Eq a => a -> [(a, b)] -> b
lookup' x xys = fromJust $ lookup x xys

matchGraphs :: HostGraph -> RuleGraph -> [GraphMorphism]
matchGraphs h r = concatMap (matchGraphEdges h r) $ matchGraphNodes h r


-- Outputs all valid (w.r.t labels) injective morphisms from the nodes of LHS
-- to the nodes of the host graph.
-- We generate all candidate sets of nodes in the host graph. For an LHS with
-- k nodes, this is the set of size-k subsets of the node set of the host graph,
-- including permutations.
-- These subsets are zipped with the node set of the LHS to create sets of 
-- candidate morphisms. These are tested with doNodesMatch, called by labMatch.
-- labMatch returns a Maybe Environment. We use the maybe function to lift
-- each Maybe Environment into a Maybe NodeMorphism, then call catMaybes
-- on the resulting list.

matchGraphNodes :: HostGraph -> RuleGraph -> [NodeMorphism]
matchGraphNodes h r = 
    catMaybes [ nm | nodeSet <- nodeSets, 
          let nodeMatches = zip rns nodeSet
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
                  substMerge mapping env

                
doNodesMatch :: HostGraph -> RuleGraph -> HostNodeId -> RuleNodeId -> Maybe Environment
doNodesMatch h r hid rid = 
   let hnode = (maybeNLabel h hid)
       rnode = (maybeNLabel r rid) in
   case (hnode, rnode) of 
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just (HostNode _ _ hlabel), Just (RuleNode _ _ rlabel)) 
                     -> doLabelsMatch hlabel rlabel


-- For each edge in the RuleGraph, generate a pair of its source and target node.
-- Each of these pairs is transformed to the source and target node in the host
-- graph using the NodeMorphism.
-- joiningEdges is called on each pair of HostNodeIds to generate all the candidate
-- edges for each RuleEdge. A RuleEdge may have more than one candidate
-- HostEdge, so a list of lists is created.
-- I use this list of lists to generate the EdgeMatches which are passed
-- to the list comprehension in the same way as in the node matcher.

matchGraphEdges :: HostGraph -> RuleGraph -> NodeMorphism -> [GraphMorphism]
matchGraphEdges h r (NM env nodeMatches) =
   catMaybes [ gm | maybeEnv = foldr labelMatch (Just []) edgeMatches
                    gm = maybe Nothing (\env -> Just (GM env edgeMatches) ) maybeEnv ]
   where 
     -- [RuleEdgeId]
     ruleEdges = allEdges r
     -- [(RuleNodeId, RuleNodeId)]
     ruleEndPoints = map (\e -> (source r e, target r e)) ruleEdges
     -- [(HostNodeId, HostNodeId)]
     hostEndPoints = map ruleEndsToHostEnds ruleEndPoints
     
     ruleEndsToHostEnds :: (RuleNodeId, RuleNodeId) -> (HostNodeId, HostNodeId)
     ruleEndsToHostEnds (src, tgt) = (lookup' src nodeMatches, lookup' tgt nodeMatches)
    
     -- [[HostEdgeId]] 
     -- Each [HostEdgeId] corresponds to one of the HostNodeId pairs in hostEndPoints
     hostEdges = map (\(src, tgt) -> [eid | eid <- joiningEdges h src tgt]) hostEndPoints

     -- I want to do the following:
     -- edgeMatches [E 1, E 2] [[E 1],[E 2, E 3]] = [(E 1, E 1), (E 2, E 2), (E 2, E 3)]
     -- edgeMatches [E 1, E 2] [[E 1, E 3],[E 2, E 3]] = [(E 1, E 1), (E 1, E 3), (E 2, E 2), (E 2, E 3)]
     -- [(RuleEdgeId, HostEdgeId)]
     edgeMatches = someZippyFunction ruleEdges hostEdges

     labelMatch :: (RuleEdgeId, HostEdgeId) -> Maybe Environment -> Maybe Environment
     labelMatch (re, he) menv = do
               env <- menv
               mapping <- doEdgesMatch h r he re
               substMerge mapping env
            
doEdgesMatch :: HostGraph -> RuleGraph -> HostEdgeId -> RuleEdgeId -> Maybe Environment
doEdgesMatch h r hid rid = 
   let mhlabel = (maybeELabel h hid) in
       mrlabel = (maybeELabel r rid) in
   case (mhlabel, mrlabel) of 
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just hlabel, Just rlabel) -> doLabelsMatch hlabel rlabel
        
-- For each pair of nodes in the node morphism, we add the morphisms for any edges
-- connecting these nodes together. This is achieved similarly to matchGraphNodes
-- by filtering the set of all possible candidate edge morphisms.
-- The candidate set is generated using the sets of outgoing edges from the rule node 
-- and the host node in the current node morphism.

{-
matchGraphEdges :: HostGraph -> RuleGraph -> NodeMorphism -> [GraphMorphism]
matchGraphEdges h r (NM env nodeMatches) = concatMap getGMsForNode nodeMatches
    where
        getGMsForNode :: (RuleNodeId, HostNodeId) -> [GraphMorphism]
        getGMsForNode (rn, hn) =
          catMaybes [ gm | edgeSet <- edgeSets,
                let edgeMatches = zip res edgeSet
                    maybeEnv = foldr labMatch (Just env) edgeMatches
                    gm = maybe Nothing (\e -> Just (GM e nodeMatches edgeMatches) ) maybeEnv ]
            where
                res = outEdges r rn
                -- Find corresponding host graph node, and all size-n permuted subsets of outgoing edges. 
                hes = outEdges h hn
                edgeSets = permutedSizedSubsets (length res) hes

        -- Match edge labels, further building the environment of variable-value mappings.
        labMatch :: (RuleEdgeId, HostEdgeId) -> Maybe (Environment) -> Maybe (Environment)
        labMatch (re, he) env = do
            e <- env
            e' <- doEdgesMatch h r he re
            substMerge e e'
-}




