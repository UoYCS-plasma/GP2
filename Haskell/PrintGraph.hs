module PrintGraph (printHostGraph,printGraph,printHostAtom) where

import Graph
import GPSyntax
import Data.Char(toLower)
import Data.List(intersperse)

printHostGraph :: HostGraph -> String
printHostGraph = printGraph printHostNode printHostLabel

printGraph :: (a->String) -> (b->String) -> Graph a b -> String
printGraph nShow eShow g = "[\n" ++ nodeList g ++ "|\n" ++ edgeList g ++ "]\n"
  where
  nodeList g  =  concatMap prettyNode $ allNodes g
  edgeList g  =  concatMap prettyEdge $ allEdges g
  prettyNode (n,nData) =
    " (n" ++ show (nodeNumber n) ++ nShow nData ++ ")\n"
  prettyEdge (e,eData) =
    " (e" ++ show (edgeNumber e) ++ ", "
    ++ "n" ++ show (nodeNumber $ source e) ++ ", "
    ++ "n" ++ show (nodeNumber $ target e)
    ++ eShow eData ++ ")\n"

printHostNode :: HostNode -> String
printHostNode (HostNode _ root label) =
  (if root then " (R)" else "") ++ printHostLabel label 

printHostLabel :: HostLabel -> String
printHostLabel (HostLabel atoms colour) =
  ", " ++ (if null atoms then "empty"
           else concat $ intersperse ":" $ map printHostAtom atoms)
       ++ printColour colour

printHostAtom :: HostAtom -> String
printHostAtom (Int i)  =  show i
printHostAtom (Chr c)  =  show c
printHostAtom (Str s)  =  show s

printColour :: Colour -> String
printColour Uncoloured  =  ""
printColour c           =  " # " ++ map toLower (show c)

