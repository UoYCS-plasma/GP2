-- a simple implementation of labelled graphs using sparse arrays for node and edge sets
-- Colin Runciman (colin.runciman@york.ac.uk) April 2014

module Graph (Graph, NodeId, EdgeId,
               emptyGraph, newNode, newNodeList, newEdge, newEdgeList,
               allNodes, outEdges, inEdges, joiningEdges, allEdges,
               maybeSource, source, maybeTarget, target, 
               maybeNLabel, nLabel, maybeELabel, eLabel,
               rmNode, rmIsolatedNodeList, rmEdge, rmEdgeList,
               eReLabel, nReLabel, dumpGraphViz) where

import Prelude hiding (lookup)
import ExAr
import Data.Maybe
import Data.List (union, intersect)

dumpGraphViz :: (Show a, Show b) => Graph a b -> String
dumpGraphViz g = gvHeader ++ prettyNodes g ++ "\n" ++ prettyEdges g ++ gvFooter
    where
        gvHeader = "digraph {\n"
        gvFooter = "}\n"
        --prettyNodes _ = "wanker"
        prettyNodes g = concatMap prettyNode $ allNodes g
        prettyEdges g = concatMap prettyEdge $ allEdges g
        prettyNode n@(N id) = "\tnode_" ++ show id ++ "\t{ label=\"" ++ show ( nLabel g n) ++ "\" }\n"
        prettyEdge e@(E id) = "\tnode_" ++ getNodeIdAsInt (source g e)
                        ++ " -> node_" ++ getNodeIdAsInt (target g e)
                        ++ "\t{ label=\"" ++ show (eLabel g e) ++ "\" }\n"
        getNodeIdAsInt (N id) = show id


-- labelled graphs
data Graph a b = Graph (ExAr Int (Node a)) (ExAr Int (Edge b)) deriving Show

-- intended data invariant for Graph values
invGraph :: Graph a b -> Bool
invGraph (Graph ns es)  =  null $ findAll invalidEdge es
  where
  d  =  domain ns
  invalidEdge (Edge (N i) (N j) _)  =  notElem i d || notElem j d

newtype NodeId = N Int deriving (Eq, Show)
newtype EdgeId = E Int deriving (Eq, Show)

data Node a = Node a               deriving Show
data Edge a = Edge NodeId NodeId a deriving Show
 
emptyGraph :: Graph a b
emptyGraph  =  Graph empty empty

newNode :: Graph a b -> a -> (Graph a b, NodeId)
newNode (Graph ns es) x  =  (Graph ns' es, N i)
  where
  (ns', i)  =  extend ns (Node x)

newNodeList :: Graph a b -> [a] -> (Graph a b, [NodeId])
newNodeList g xs = foldr addNode (g, []) xs
  where 
  addNode :: a -> (Graph a b, [NodeId]) -> (Graph a b, [NodeId])
  addNode label (g, nids) = (g', nid:nids) where (g', nid) = newNode g label 

newEdge :: Graph a b -> NodeId -> NodeId -> b -> (Graph a b, EdgeId)
newEdge (Graph ns es) n1 n2 x  =  (Graph ns es', E i)
  where
  (es', i)  =  extend es (Edge n1 n2 x)

newEdgeList :: Graph a b -> [(NodeId, NodeId, b)] -> (Graph a b, [EdgeId])
newEdgeList g xs = foldr addEdge (g, []) xs
  where 
  addEdge :: (NodeId, NodeId, b) -> (Graph a b, [EdgeId]) -> (Graph a b, [EdgeId])
  addEdge (src, tgt, lab) (g, eids) = (g', eid:eids) where (g', eid) = newEdge g src tgt lab

allNodes :: Graph a b -> [NodeId]
allNodes (Graph ns _)  =  map N (domain ns)

allEdges :: Graph a b -> [EdgeId]
allEdges (Graph _ es) = map E (domain es)

outEdges :: Graph a b -> NodeId -> [EdgeId]
outEdges (Graph _ es) n  =  map E $ findAll (\(Edge n1 _ _) -> n1 == n) es

inEdges :: Graph a b -> NodeId -> [EdgeId]
inEdges (Graph _ es) n  =  map E $ findAll (\(Edge _ n2 _) -> n2 == n) es

joiningEdges :: Graph a b -> NodeId -> NodeId -> [EdgeId]
joiningEdges (Graph _ es) src tgt = map E $ findAll (\(Edge n1 n2 _) -> n1 == src && n2 == tgt) es

maybeSource :: Graph a b -> EdgeId -> Maybe NodeId
maybeSource (Graph _ es) (E i)  =
  maybe Nothing (\(Edge n1 _ _) -> Just n1) (idLookup es i)

source :: Graph a b -> EdgeId -> NodeId
source g eid = fromJust $ maybeSource g eid

maybeTarget :: Graph a b -> EdgeId -> Maybe NodeId
maybeTarget (Graph _ es) (E i)  =
  maybe Nothing (\(Edge _ n2 _) -> Just n2) (idLookup es i)

target :: Graph a b -> EdgeId -> NodeId
target g eid = fromJust $ maybeTarget g eid

maybeNLabel :: Graph a b -> NodeId -> Maybe a
maybeNLabel (Graph ns _) (N i)  =
  maybe Nothing (\(Node x) -> Just x) (idLookup ns i)

nLabel :: Graph a b -> NodeId -> a
nLabel g nid = fromJust $ maybeNLabel g nid

maybeELabel :: Graph a b -> EdgeId -> Maybe b
maybeELabel (Graph _ es) (E i)  =
  maybe Nothing (\(Edge _ _ x) -> Just x) (idLookup es i)

eLabel :: Graph a b -> EdgeId -> b
eLabel g eid = fromJust $ maybeELabel g eid

-- removing a node also removes all edges with the node as source or target
rmNode :: Graph a b -> NodeId -> Graph a b
rmNode (Graph ns es) n@(N i)  =  Graph ns' es'
  where
  ns'  =  remove ns i
  es'  =  removeAll (\(Edge n1 n2 _) -> n1 == n || n2 == n) es

-- returns Nothing if any of the NodeIds have incident edges.
rmIsolatedNodeList :: Graph a b -> [NodeId] -> Maybe (Graph a b)
rmIsolatedNodeList g nids = foldr deleteIsolatedNode (Just g) nids
  where 
  deleteIsolatedNode :: NodeId -> Maybe (Graph a b) -> Maybe (Graph a b)
  deleteIsolatedNode nid Nothing  = Nothing
  deleteIsolatedNode nid (Just g) = if null $ union (outEdges g nid) (inEdges g nid) 
                                    then Just (rmNode g nid)
                                    else Nothing

rmEdge :: Graph a b -> EdgeId -> Graph a b
rmEdge (Graph ns es) (E i)  =  Graph ns es'
  where
  es'  =  remove es i

rmEdgeList :: Graph a b -> [EdgeId] -> Graph a b
rmEdgeList g eids = foldr (flip rmEdge) g eids

eReLabel :: Graph a b -> EdgeId -> b -> Graph a b
eReLabel (Graph ns es) (E i) x  =  Graph ns es'
  where
  es'  =  update (\(Edge n1 n2 _) -> Edge n1 n2 x) es i

nReLabel :: Graph a b -> NodeId -> a -> Graph a b
nReLabel (Graph ns es) (N i) x  =  Graph ns' es
  where
  ns'  =  update (\(Node _) -> Node x) ns i

