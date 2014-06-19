-- a simple implementation of labelled graphs using sparse arrays for node and edge sets
-- Colin Runciman (colin.runciman@york.ac.uk) April 2014

module Graph (Graph, NodeId, EdgeId, pretty,
               emptyGraph, newNode, newEdge,
               allNodes, outEdges, inEdges, allEdges,
               source, target, nLabel, eLabel,
               rmNode, rmEdge, eReLabel, nReLabel) where

import Prelude hiding (lookup)
import ExAr
import Data.Maybe
import Data.List (intersect)

class Pretty a
instance Pretty (Graph a b) where
pretty :: (Show a, Show b) => Graph a b -> String
pretty g = gvHeader ++ prettyNodes g ++ "\n" ++ prettyEdges g ++ gvFooter
    where
        gvHeader = "digraph {\n"
        gvFooter = "}\n"
        prettyNodes g = concatMap prettyNode $ allNodes g
        prettyEdges g = concatMap prettyEdge $ allEdges g
        prettyNode n@(N id) = "\tnode_" ++ show id ++ "\t{ label=\"" ++ show ( fromJust (nLabel g n) ) ++ "\" }\n"
        prettyEdge e@(E id) = "\tnode_" ++ getNodeIdAsInt ( fromJust (source g e) )
                         ++ " -> node_" ++ getNodeIdAsInt ( fromJust (target g e) )
                         ++ "\t{ label=\"" ++ show ( fromJust (eLabel g e) ) ++ "\" }\n"
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

newEdge :: Graph a b -> NodeId -> NodeId -> b -> (Graph a b, EdgeId)
newEdge (Graph ns es) n1 n2 x  =  (Graph ns es', E i)
  where
  (es', i)  =  extend es (Edge n1 n2 x)

allNodes :: Graph a b -> [NodeId]
allNodes (Graph ns _)  =  map N (domain ns)

allEdges :: Graph a b -> [EdgeId]
allEdges (Graph _ es) = map E (domain es)

outEdges :: Graph a b -> NodeId -> [EdgeId]
outEdges (Graph _ es) n  =  map E $ findAll (\(Edge n1 _ _) -> n1 == n) es

inEdges :: Graph a b -> NodeId -> [EdgeId]
inEdges (Graph _ es) n  =  map E $ findAll (\(Edge _ n2 _) -> n2 == n) es

source :: Graph a b -> EdgeId -> Maybe NodeId
source (Graph _ es) (E i)  =
  maybe Nothing (\(Edge n1 _ _) -> Just n1) (idLookup es i)

target :: Graph a b -> EdgeId -> Maybe NodeId
target (Graph _ es) (E i)  =
  maybe Nothing (\(Edge _ n2 _) -> Just n2) (idLookup es i)

nLabel :: Graph a b -> NodeId -> Maybe a
nLabel (Graph ns _) (N i)  =
  maybe Nothing (\(Node x) -> Just x) (idLookup ns i)

eLabel :: Graph a b -> EdgeId -> Maybe b
eLabel (Graph _ es) (E i)  =
  maybe Nothing (\(Edge _ _ x) -> Just x) (idLookup es i)

-- removing a node also removes all edges with the node as source or target
rmNode :: Graph a b -> NodeId -> Graph a b
rmNode (Graph ns es) n@(N i)  =  Graph ns' es'
  where
  ns'  =  remove ns i
  es'  =  removeAll (\(Edge n1 n2 _) -> n1 == n || n2 == n) es

rmEdge :: Graph a b -> EdgeId -> Graph a b
rmEdge (Graph ns es) (E i)  =  Graph ns es'
  where
  es'  =  remove es i

eReLabel :: Graph a b -> EdgeId -> b -> Graph a b
eReLabel (Graph ns es) (E i) x  =  Graph ns es'
  where
  es'  =  update (\(Edge n1 n2 _) -> Edge n1 n2 x) es i

nReLabel :: Graph a b -> NodeId -> a -> Graph a b
nReLabel (Graph ns es) (N i) x  =  Graph ns' es
  where
  ns'  =  update (\(Node _) -> Node x) ns i

