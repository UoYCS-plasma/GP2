module GraphViz where

import Data.List

import GPSyntax
import Graph

showNodeId :: NodeId -> String
showNodeId = filter (/= ' ') . show

showEdgeId :: EdgeId -> String
showEdgeId = filter (/= ' ') . show

drawGraph :: HostGraph -> String
drawGraph g = "digraph {\n\tgraph [ autosize=false size=\"8,8!\" ratio=fill ];\nnode [ shape=circle ];\n\n\t" ++ nodeStr ++ "\n\n\t" ++ edgeStr ++ "\n}"
    where
        nodeStr = intercalate "\n\t" $ map (drawNode g) $ allNodes g
        edgeStr = intercalate "\n\t" $ map (drawEdge g) $ allEdges g

-- data HostEdge = HostEdge NodeName NodeName HostLabel deriving Show
drawEdge :: HostGraph -> EdgeId -> String
drawEdge g id = src ++ " -> " ++ dst ++ " [ " ++ drawLabel label ++ " ];"
    where
        src = showNodeId $ source g id
        dst = showNodeId $ target g id
        label = eLabel g id

-- data HostNode = HostNode NodeName Bool HostLabel deriving Show
drawNode :: HostGraph -> NodeId -> String
drawNode g id = case nLabel g id of
        HostNode _ False  label -> name ++ " [ " ++ drawLabel label ++ " ];"
        HostNode _ True label -> name ++ " [ penwidth=5 ; " ++ drawLabel label ++ " ];"
    where
        name = showNodeId id


-- data HostLabel = HostLabel [HostAtom] Colour deriving (Eq, Show)
drawLabel :: HostLabel -> String
drawLabel (HostLabel atoms Uncoloured) = "label=\"" ++ (intercalate ":" $ map drawAtom atoms) ++ "\""
drawLabel (HostLabel atoms col) = "label=\"" ++ (intercalate ":" $ map drawAtom atoms )
    ++ "\" " ++ " style=filled color=" ++ show col

drawAtom :: HostAtom -> String
drawAtom = show




