-- a simple implementation of labelled graphs using sparse arrays for node and edge sets
-- Colin Runciman (colin.runciman@york.ac.uk) April 2014
-- Modifications by Chris Bak 26 June 2014

module Graph (Graph, NodeId, EdgeId, pretty,
               emptyGraph, newNode, newEdge,
               allNodes, outEdges, inEdges, joiningEdges, allEdges,
               source, target, nLabel, eLabel,
               rmNode, rmEdge, eReLabel, nReLabel) where

-- Prelude.lookup was hidden before but I renamed the ExAr lookup to avoid clashes
-- in other modules.
import Prelude
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
-- Changed ExArs to take Strings to establish direct correspondence with IDs from
-- text file.
data Graph a b = Graph (ExAr (Node a)) (ExAr (Edge b)) deriving Show

-- intended data invariant for Graph values
invGraph :: Graph a b -> Bool
invGraph (Graph ns es)  =  null $ findAll invalidEdge es
  where
  d  =  domain ns
  invalidEdge (Edge (N i) (N j) _)  =  notElem i d || notElem j d

newtype NodeId = N String deriving (Eq, Show)
newtype EdgeId = E String deriving (Eq, Show)

data Node a = Node a               deriving Show
data Edge a = Edge NodeId NodeId a deriving Show
 
emptyGraph :: Graph a b
emptyGraph  =  Graph empty empty

-- Now called with a String containing the NodeId
newNode :: Graph a b -> String -> a -> (Graph a b, NodeId)
newNode (Graph ns es) id x  =  (Graph ns' es, N id)
  where
  (ns', id)  =  extend ns id (Node x)

-- Now called with a String containing the EdgeId
newEdge :: Graph a b -> String -> NodeId -> NodeId -> b -> (Graph a b, EdgeId)
newEdge (Graph ns es) id n1 n2 x  =  (Graph ns es', E id)
  where
  (es', id)  =  extend es id (Edge n1 n2 x)

allNodes :: Graph a b -> [NodeId]
allNodes (Graph ns _)  =  map N (domain ns)

allEdges :: Graph a b -> [EdgeId]
allEdges (Graph _ es) = map E (domain es)

outEdges :: Graph a b -> NodeId -> [EdgeId]
outEdges (Graph _ es) n  =  map E $ findAll (\(Edge src _ _) -> src == n) es

inEdges :: Graph a b -> NodeId -> [EdgeId]
inEdges (Graph _ es) n  =  map E $ findAll (\(Edge _ tgt _) -> tgt == n) es

-- New function to generate the set of edges connecting two nodes in a particular direction.
joiningEdges :: Graph a b -> NodeId -> NodeId -> [EdgeId]
joiningEdges (Graph _ es) src tgt = map E $ findAll (\(Edge s t _) -> s == src && t == tgt) es

source :: Graph a b -> EdgeId -> Maybe NodeId
source (Graph _ es) (E id)  =
  maybe Nothing (\(Edge src _ _) -> Just src) (idLookup es id)

target :: Graph a b -> EdgeId -> Maybe NodeId
target (Graph _ es) (E id)  =
  maybe Nothing (\(Edge _ tgt _) -> Just tgt) (idLookup es id)

nLabel :: Graph a b -> NodeId -> Maybe a
nLabel (Graph ns _) (N id)  =
  maybe Nothing (\(Node x) -> Just x) (idLookup ns id)

eLabel :: Graph a b -> EdgeId -> Maybe b
eLabel (Graph _ es) (E id)  =
  maybe Nothing (\(Edge _ _ x) -> Just x) (idLookup es id)

-- removing a node also removes all edges with the node as source or target
rmNode :: Graph a b -> NodeId -> Graph a b
rmNode (Graph ns es) n@(N id)  =  Graph ns' es'
  where
  ns'  =  remove ns id
  es'  =  removeAll (\(Edge src tgt _) -> src == n || tgt == n) es

rmEdge :: Graph a b -> EdgeId -> Graph a b
rmEdge (Graph ns es) (E id)  =  Graph ns es'
  where
  es'  =  remove es id

eReLabel :: Graph a b -> EdgeId -> b -> Graph a b
eReLabel (Graph ns es) (E id) x  =  Graph ns es'
  where
  es'  =  update (\(Edge src tgt _) -> Edge src tgt x) es id

nReLabel :: Graph a b -> NodeId -> a -> Graph a b
nReLabel (Graph ns es) (N id) x =  Graph ns' es
  where
  ns'  =  update (\(Node _) -> Node x) ns id

