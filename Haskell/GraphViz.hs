module GraphViz where

import GPSyntax

drawGraph :: GPGraph -> String

-- data HostEdge = HostEdge NodeName NodeName HostLabel deriving Show
drawEdge :: HostEdge -> String
drawEdge (HostEdge src dst label) = src ++ " -> " ++ dst
    ++ " [ " ++ drawLabel label ++ " ];"

-- data HostNode = HostNode NodeName Bool HostLabel deriving Show
drawNode :: HostNode -> String
drawNode (HostNode name False label) = name ++ " [ " ++ drawLabel label ++ " ];"
drawNode (HostNode name True label) = name ++ " [ shape=diamond; " ++ drawLabel label ++ " ];"


-- data HostLabel = HostLabel [HostAtom] Colour deriving (Eq, Show)
drawLabel :: HostLabel -> String
drawLabel (HostLabel [] col) = "style=filled color=" ++ show col
drawLabel (HostLabel atoms col) = "label=\"" ++ (intercalate ":" $ map drawAtom atoms )
    ++ "\"; " ++ " style=filled color=" ++ show col

drawAtom :: HostAtom -> String
drawAtom = show 




