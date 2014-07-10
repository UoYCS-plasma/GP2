-- labelled graph isomorphism test
-- Colin Runciman, July 2014

module GraphIsomorphism (getIsomorphismData) where

import Data.List (permutations)
import Graph
import Mapping


-- Used after applying a RuleCall to a graph g, giving output graphs hs.
-- k is the current count of isomorphic copies of g.

-- getIsomorphismData (g, k) hs does the following:
-- (1) Count the graphs in hs isomorphic to g. Let's say there are m such graphs. 
--     Then update (g, k) to (g, km): for each instance of g, m copies are generated,
--     giving a total of km isomorphic copies of g. If there are no such graphs,
--     do not include (g, k) in the output list.
-- (2) Call isomorphismCount on the graphs in hs not isomorphic to g. This outputs
--     the isomorphism count of those graphs. Let's call that hs'
-- (3) Return (g, km) : hs'
getIsomorphismData :: (Eq a, Eq b) => (Graph a b, Int) -> [Graph a b] -> [(Graph a b, Int)]
getIsomorphismData (g, k) hs = 
    let partition = isomorphicSet (g:hs) 
        -- isomorphicSet (g:hs) places g at the head of the list of graphs from hs
        -- isomorphic to g. g is not counted here.
        gs        = tail $ fst partition
        hs'       = snd partition 
        ihs' = isomorphismCount hs' in
     if null gs then ihs' else (g, k*length gs) : ihs'

-- Given a list of graphs, isomorphismCount returns a list of pairs.
-- Each pair contains a single representative of a set of isomorphic graphs in 
-- the list and a count of how many isomorphic copies of that graph were in the
-- input list.
isomorphismCount :: (Eq a, Eq b) => [Graph a b] -> [(Graph a b, Int)]
isomorphismCount graphs = [ (g, length (g:gs)) | (g:gs) <- isoGraphs] 
    where isoGraphs = isomorphicPartition graphs

-- Uses isomorphicSet to partition a list of graphs into a list of lists of 
-- graphs such that each list contains a number of isomorphic copies of a graph
-- taken from the input list.
isomorphicPartition :: (Eq a, Eq b) => [Graph a b] -> [[Graph a b]]
isomorphicPartition [] = [] 
isomorphicPartition graphs = copiesOfHead : isomorphicPartition rest
    where (copiesOfHead, rest) = isomorphicSet graphs

-- Partitions a list of graphs gs into two lists.
-- The first list is all graphs isomorphic to, and including, head gs
-- The second list is the rest of the input list.
isomorphicSet :: (Eq a, Eq b) => [Graph a b] -> ([Graph a b], [Graph a b])
isomorphicSet [] = ([],[])
isomorphicSet (g:gs) = (copies, rest)
    where copies = g : filter (isomorphic g) gs
          rest   = filter (not . isomorphic g) gs


isomorphic :: (Eq a, Eq b) => Graph a b -> Graph a b -> Bool
isomorphic g1 g2 =
  length ns1 == length ns2 &&
  any (edgesIso g1 g2) (permutationsWrt nLabel g1 g2 ns1 ns2)
  where
  ns1 = allNodes g1
  ns2 = allNodes g2

edgesIso :: (Eq a, Eq b) => Graph a b -> Graph a b -> Mapping NodeId NodeId -> Bool
edgesIso g1 g2 s = all (outEdgesIso g1 g2 s) (allNodes g1)

outEdgesIso :: (Eq a, Eq b) => Graph a b -> Graph a b -> Mapping NodeId NodeId -> NodeId -> Bool
outEdgesIso g1 g2 s n1 =
  length es1 == length es2 &&
  any (targetsIso g1 g2 s) (permutationsWrt eLabel g1 g2 es1 es2) 
  where
  n2  = definiteLookup n1 s
  es1 = outEdges g1 n1
  es2 = outEdges g2 n2

targetsIso :: Eq b => Graph a b -> Graph a b -> Mapping NodeId NodeId -> Mapping EdgeId EdgeId -> Bool
targetsIso g1 g2 sn se = all sameTarget se
  where
  sameTarget (e1, e2) = definiteLookup t1 sn == t2
    where
    t1 = target g1 e1
    t2 = target g2 e2

permutationsWrt :: Eq d => (Graph a b -> c -> d) -> Graph a b -> Graph a b -> [c] -> [c] -> [Mapping c c]
permutationsWrt f g1 g2 xs1 xs2 =
  filter (all agree) [zip xs1 xs2' | xs2' <- permutations xs2]
  where
  agree (x1, x2)  =  f g1 x1 == f g2 x2




