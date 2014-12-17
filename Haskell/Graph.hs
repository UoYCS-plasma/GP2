-- a simple implementation of labelled graphs using Data.Map
-- for integer-indexed sets of nodes and edges
-- Colin Runciman (colin.runciman@york.ac.uk) April 2014

module Graph (Graph, NodeId, EdgeId, nodeNumber, edgeNumber,
              emptyGraph, newNode, newNodeList, newEdge, newEdgeList,
              allNodes, allEdges, outEdges, outdegree, inEdges, indegree, 
              incidentEdges, joiningEdges,
              source, target, nLabel, eLabel,
              rmNode, rmNodeList, rmEdge, rmEdgeList, eReLabel, nReLabel)
              where

import Prelude hiding (lookup, filter)
import Data.Map hiding (map, union, foldr)
import Data.List (union)

-- labelled graphs: edges in the range of the edge map should
-- contain end-points in the range of the node map; the Int
-- values are next nodeNumber and next edgeNumber.
data Graph a b = Graph (Map Int (Node a)) Int (Map Int (Edge b)) Int
                 deriving Show

data Node a = Node a               deriving Show
data Edge a = Edge NodeId NodeId a deriving Show
 
newtype NodeId = N Int deriving (Ord, Eq, Show)
newtype EdgeId = E Int deriving (Ord, Eq, Show)

nodeNumber :: NodeId -> Int
nodeNumber (N i) = i

edgeNumber :: EdgeId -> Int
edgeNumber (E i) = i

emptyGraph :: Graph a b
emptyGraph = Graph empty 1 empty 1

newNode :: Graph a b -> a -> (Graph a b, NodeId)
newNode (Graph ns i es j) x  =
  (Graph (insert i (Node x) ns) (i+1) es j, N i)

newNodeList :: Graph a b -> [a] -> (Graph a b, [NodeId])
newNodeList g xs = foldr addNode (g, []) xs
  where 
  addNode :: a -> (Graph a b, [NodeId]) -> (Graph a b, [NodeId])
  addNode label (g, nids) = (g', nid:nids) where (g', nid) = newNode g label 

newEdge :: Graph a b -> NodeId -> NodeId -> b -> (Graph a b, EdgeId)
newEdge (Graph ns i es j) n1 n2 x  =
  (Graph ns i (insert j (Edge n1 n2 x) es) (j+1), E j)

newEdgeList :: Graph a b -> [(NodeId, NodeId, b)] -> (Graph a b, [EdgeId])
newEdgeList g xs = foldr addEdge (g, []) xs
  where 
  addEdge :: (NodeId, NodeId, b) -> (Graph a b, [EdgeId]) -> (Graph a b, [EdgeId])
  addEdge (src, tgt, lab) (g, eids) = (g', eid:eids) where (g', eid) = newEdge g src tgt lab

allNodes :: Graph a b -> [NodeId]
allNodes (Graph ns _ _ _)  =  map N (keys ns)

allEdges :: Graph a b -> [EdgeId]
allEdges (Graph _ _ es _)  =  map E (keys es)

outEdges :: Graph a b -> NodeId -> [EdgeId]
outEdges (Graph _ _ es _) n  =  map E $ keys $ filter (hasSrc n) es
  where
  hasSrc n (Edge n1 _ _)  =  n1 == n

outdegree :: Graph a b -> NodeId -> Int
outdegree g n = length $ outEdges g n

inEdges :: Graph a b -> NodeId -> [EdgeId]
inEdges (Graph _ _ es _) n  =  map E $ keys $ filter (hasTgt n) es
  where
  hasTgt n (Edge _ n2 _)  =  n2 == n

indegree :: Graph a b -> NodeId -> Int
indegree g n = length $ inEdges g n

incidentEdges :: Graph a b -> NodeId -> [EdgeId]
incidentEdges g n = outEdges g n `union` inEdges g n

joiningEdges :: Graph a b -> NodeId -> NodeId -> [EdgeId]
joiningEdges (Graph _ _ es _) src tgt  =
  map E $ keys $ filter (hasSrcTgt src tgt) es
  where
  hasSrcTgt src tgt (Edge n1 n2 _)  =  n1 == src && n2 == tgt

source :: Graph a b -> EdgeId -> NodeId
source (Graph _ _ es _) (E i)  =  let Edge src _ _ = es ! i in src

target :: Graph a b -> EdgeId -> NodeId
target (Graph _ _ es _) (E i)  =  let Edge _ tgt _ = es ! i in tgt

eLabel :: Graph a b -> EdgeId -> b
eLabel (Graph _ _ es _) (E i)  =  let Edge _ _ lab = es ! i in lab

nLabel :: Graph a b -> NodeId -> a
nLabel (Graph ns _ _ _) (N i)  =  let Node lab = ns ! i in lab

-- removing a node also removes all edges incident to it
rmNode :: Graph a b -> NodeId -> Graph a b
rmNode (Graph ns i es j) n@(N k)  =
  Graph (delete k ns) i (filter (not . incidentTo n) es) j
  where
  incidentTo n (Edge n1 n2 _)  =  n1 == n || n2 == n

rmNodeList :: Graph a b -> [NodeId] -> Graph a b
rmNodeList g nids  =  foldr (flip rmNode) g nids

rmEdge :: Graph a b -> EdgeId -> Graph a b
rmEdge (Graph ns i es j) (E k)  =  Graph ns i (delete k es) j

rmEdgeList :: Graph a b -> [EdgeId] -> Graph a b
rmEdgeList g eids  =  foldr (flip rmEdge) g eids

eReLabel :: Graph a b -> EdgeId -> b -> Graph a b
eReLabel (Graph ns i es j) (E k) x  =
  Graph ns i (adjust (relabel x) k es) j
  where
  relabel x (Edge n1 n2 _)  =  Edge n1 n2 x

nReLabel :: Graph a b -> NodeId -> a -> Graph a b
nReLabel (Graph ns i es j) (N k) x  =
  Graph (adjust (relabel x) k ns) i es j
  where
  relabel x (Node _)  =  Node x

