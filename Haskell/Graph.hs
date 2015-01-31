-- a simple implementation of labelled graphs using
-- integer-indexed sets of nodes and edges
-- Colin Runciman (colin.runciman@york.ac.uk) April 2014
-- Nov 2014: use Data.Map instead of association lists 
-- Jan 2015: allNodes and allEdges list key-data pairs
-- not just keys; edges indexed by source,target,edge-no

module Graph (Graph, NodeKey, EdgeKey, nodeNumber, source, target, edgeNumber,
              emptyGraph, newNode, newNodeList, newEdge, newEdgeList,
              allNodeKeys, allNodes, allEdgeKeys, allEdges, nLabel, eLabel,
              outEdges, outdegree, inEdges, indegree, joiningEdges, incidentEdges, 
              rmNode, rmNodeList, rmEdge, rmEdgeList, eReLabel, nReLabel)
              where

import qualified Data.Map as Map

newtype NodeKey  =  N {nodeNumber :: Int}
                    deriving (Ord, Eq, Show)
data    EdgeKey  =  E {source, target :: NodeKey, edgeNumber :: Int}
                    deriving (Ord, Show)

instance Eq EdgeKey where (E _ _ i) == (E _ _ j)  =  i == j

-- The Int values here are next nodeNumber and next edgeNumber.
data Graph a b = Graph (Map.Map NodeKey a) Int (Map.Map EdgeKey b) Int
                 deriving Show

emptyGraph :: Graph a b
emptyGraph = Graph Map.empty 1 Map.empty 1

newNode :: Graph a b -> a -> (Graph a b, NodeKey)
newNode (Graph ns i es j) x  =
  (Graph (Map.insert nk x ns) (i+1) es j, nk) where nk  =  N i

newNodeList :: Graph a b -> [a] -> (Graph a b, [NodeKey])
newNodeList g xs  =  foldr addNode (g, []) xs
  where addNode label (g, nks)  =
          (g', nk:nks) where (g', nk) = newNode g label 

newEdge :: Graph a b -> NodeKey -> NodeKey -> b -> (Graph a b, EdgeKey)
newEdge (Graph ns i es j) n1 n2 x  =
  (Graph ns i (Map.insert ek x es) (j+1), ek) where ek  =  E n1 n2 j

newEdgeList :: Graph a b -> [(NodeKey, NodeKey, b)] -> (Graph a b, [EdgeKey])
newEdgeList g xs = foldr addEdge (g, []) xs
  where addEdge (src, tgt, lab) (g, eks)  =
          (g', ek:eks) where (g', ek)  =  newEdge g src tgt lab

allNodeKeys :: Graph a b -> [NodeKey]
allNodeKeys (Graph ns _ _ _)  =  Map.keys ns

allNodes :: Graph a b -> [(NodeKey,a)]
allNodes    (Graph ns _ _ _)  =  Map.assocs ns

allEdgeKeys :: Graph a b -> [EdgeKey]
allEdgeKeys (Graph _ _ es _)  =  Map.keys es

allEdges :: Graph a b -> [(EdgeKey,b)]
allEdges    (Graph _ _ es _)  =  Map.assocs es

-- internal auxiliary functions
outMap,inMap :: Graph a b -> NodeKey -> Map.Map EdgeKey b
outMap (Graph _ _ es _) n  =  Map.filterWithKey (\ek _ -> source ek == n) es
inMap  (Graph _ _ es _) n  =  Map.filterWithKey (\ek _ -> target ek == n) es

outEdges :: Graph a b -> NodeKey -> [(EdgeKey,b)]
outEdges  g n  =  Map.assocs $ outMap g n

outdegree :: Graph a b -> NodeKey -> Int
outdegree g n  =  Map.size $ outMap g n

inEdges :: Graph a b -> NodeKey -> [(EdgeKey,b)]
inEdges   g n  =  Map.assocs $ inMap g n

indegree :: Graph a b -> NodeKey -> Int
indegree  g n  =  Map.size $ inMap g n

incidentEdges :: Graph a b -> NodeKey -> [(EdgeKey,b)]
incidentEdges g n  =  outs ++ [e | e@(k,_) <- ins, source k /= n]
  where outs  =  outEdges g n  ;  ins  =  inEdges g n

joiningEdges :: Graph a b -> NodeKey -> NodeKey -> [(EdgeKey,b)]
joiningEdges (Graph _ _ es _) src tgt  =  after $ E src tgt 0
  where after ek = case Map.lookupGT ek es of
                   Nothing                 -> []
                   Just e@(ek'@(E s t _),_) ->
                     if t==tgt && s==src then e : after ek' else []

nLabel :: Graph a b -> NodeKey -> a
nLabel (Graph ns _ _ _) nk  =  ns Map.! nk

eLabel :: Graph a b -> EdgeKey -> b
eLabel (Graph _ _ es _) ek  =  es Map.! ek

-- removing a node also removes all edges incident to it
rmNode :: Graph a b -> NodeKey -> Graph a b
rmNode (Graph ns i es j) nk  =
  Graph (Map.delete nk ns) i (Map.filterWithKey notIncident es) j
  where notIncident ek _  =  source ek /= nk && target ek /= nk

rmNodeList :: Graph a b -> [NodeKey] -> Graph a b
rmNodeList g nks  =  foldr (flip rmNode) g nks

rmEdge :: Graph a b -> EdgeKey -> Graph a b
rmEdge (Graph ns i es j) ek  =  Graph ns i (Map.delete ek es) j

rmEdgeList :: Graph a b -> [EdgeKey] -> Graph a b
rmEdgeList g eks  =  foldr (flip rmEdge) g eks

nReLabel :: Graph a b -> NodeKey -> a -> Graph a b
nReLabel (Graph ns i es j) nk x  =  Graph (Map.insert nk x ns) i es j

eReLabel :: Graph a b -> EdgeKey -> b -> Graph a b
eReLabel (Graph ns i es j) ek x  =  Graph ns i (Map.insert ek x es) j

