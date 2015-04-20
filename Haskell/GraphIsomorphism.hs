-- labelled graph isomorphism test
-- Colin Runciman, July 2014; faster version December 2014

module GraphIsomorphism (isomorphismCount, isomorphic) where

import List (representBy, choices, nonEmpty, bijectionsWith)
import Data.Maybe (maybe)
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
  any (edgesIso g1 g2) (bijectionsWith (nodeAttribs g1) ns1 (nodeAttribs g2) ns2)
  where ns1 = allNodeKeys g1 ; ns2 = allNodeKeys g2

nodeAttribs :: Graph a b -> NodeKey -> (a, Int, Int)
nodeAttribs g n  =  (nLabel g n, outdegree g n, indegree g n)

edgesIso :: Ord b => Graph a b -> Graph a b -> Mapping NodeKey NodeKey -> Bool
edgesIso g1 g2 s = all (outEdgesIso g1 g2 s) (allNodeKeys g1)

outEdgesIso :: Ord b => Graph a b -> Graph a b -> Mapping NodeKey NodeKey -> NodeKey -> Bool
outEdgesIso g1 g2 s n1 =
  nonEmpty (bijectionsWith (edgeAttribs $ Just s) es1 (edgeAttribs Nothing) es2) 
  where es1 = outEdges g1 n1 ; es2 = outEdges g2 $ definiteLookup n1 s

edgeAttribs:: Maybe (Mapping NodeKey NodeKey) -> (EdgeKey,b) -> (b, NodeKey)
edgeAttribs ms (ek,elab)  =  (elab, maybe t (definiteLookup t) ms)
  where t = target ek

