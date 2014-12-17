-- labelled graph isomorphism test
-- Colin Runciman, July 2014; faster version December 2014

module GraphIsomorphism (isomorphismCount, isomorphic) where

import List (representBy, choices, nonEmpty, bijectionsWith)
import Graph
import Mapping

-- Given a list of graphs, isomorphismCount returns a list of pairs.
-- Each pair contains a single representative of a set of isomorphic graphs in 
-- the list and a count of how many isomorphic copies of that graph were in the
-- input list.
isomorphismCount :: (Ord a, Ord b) => [Graph a b] -> [(Graph a b, Int)]
isomorphismCount graphs = representBy isomorphic graphs

isomorphic :: (Ord a, Ord b) => Graph a b -> Graph a b -> Bool
isomorphic g1 g2 =
  length ns1 == length ns2 &&
  any (edgesIso g1 g2) (bijectionsWith (nodeKey g1) ns1 (nodeKey g2) ns2)
  where
  ns1 = allNodes g1
  ns2 = allNodes g2

nodeKey :: Graph a b -> NodeId -> (a, Int, Int)
nodeKey g n  =  (nLabel g n, outdegree g n, indegree g n)

edgesIso :: Ord b => Graph a b -> Graph a b -> Mapping NodeId NodeId -> Bool
edgesIso g1 g2 s = all (outEdgesIso g1 g2 s) (allNodes g1)

outEdgesIso :: Ord b => Graph a b -> Graph a b -> Mapping NodeId NodeId -> NodeId -> Bool
outEdgesIso g1 g2 s n1 =
  nonEmpty (bijectionsWith (edgeKey g1) es1 (edgeKey g2) es2) 
  where
  n2  = definiteLookup n1 s
  es1 = outEdges g1 n1
  es2 = outEdges g2 n2

edgeKey :: Graph a b -> EdgeId -> (b, NodeId)
edgeKey g e  =  (eLabel g e, target g e)

