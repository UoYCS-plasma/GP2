module GraphViz where

import Data.List
import GPSyntax
import Graph
import PrintGraph

showNodeKey :: NodeKey -> String
showNodeKey nk  =  'N' : show (nodeNumber nk)

showEdgeKey :: EdgeKey -> String
showEdgeKey ek  =  'E' : show (edgeNumber ek)

drawGraph :: HostGraph -> String
drawGraph g = "digraph {\n\tgraph [ autosize=false size=\"8,8!\" ratio=fill ];\nnode [ shape=circle ];\n\n\t" ++ nodeStr ++ "\n\n\t" ++ edgeStr ++ "\n}"
    where
        nodeStr = intercalate "\n\t" $ map drawNode $ allNodes g
        edgeStr = intercalate "\n\t" $ map drawEdge $ allEdges g

-- data HostEdge = HostEdge NodeName NodeName HostLabel deriving Show
drawEdge :: (EdgeKey,HostLabel) -> String
drawEdge (ek, label) = src ++ " -> " ++ dst ++ " [ " ++ drawLabel label ++ " ];"
    where
        src = showNodeKey $ source ek
        dst = showNodeKey $ target ek

-- data HostNode = HostNode NodeName Bool HostLabel deriving Show
drawNode :: (NodeKey,HostNode) -> String
drawNode (nk, HostNode _ root label)  =
    showNodeKey nk ++ " [ "
    ++ (if root then "penwidth=5 ; " else "")
    ++ drawLabel label ++ " ];"

-- data HostLabel = HostLabel [HostAtom] Colour deriving (Eq, Show)
drawLabel :: HostLabel -> String
drawLabel (HostLabel atoms Uncoloured) = "label=\"" ++ (intercalate ":" $ map printHostAtom atoms) ++ "\""
drawLabel (HostLabel atoms col) = "label=\"" ++ (intercalate ":" $ map printHostAtom atoms )
    ++ "\" " ++ " style=filled color=" ++ show col




