module ParseGraph where

import Data.Maybe

import ParseLib
import GPSyntax
import Graph

hostGraph :: Parser AstHostGraph
hostGraph = optSpaces |> keyword "[" |> pure AstHostGraph 
        <*> maybeSome hostNode <*> keyword "|" 
         |> maybeSome hostEdge <| keyword "]"

-- A node is a triple (Node ID, Root Node, Node Label)
-- The second component is "(R)" if root node, [] otherwise.
hostNode :: Parser HostNode
hostNode = keyword "(" |> pure HostNode
       <*> label
       <*> (root <| keyword ",") 
       <*> (hostLabel <| keyword ")")

hostEdge :: Parser HostEdge
hostEdge = keyword "(" |> pure HostEdge
       <| ( (lowerIdent <| keyword ",") )
       <*> (lowerIdent <| keyword ",")
       <*> (lowerIdent <| keyword ",")
       <*> (hostLabel <| keyword ")")

hostLabel :: Parser HostLabel
hostLabel = pure HostLabel <*> hostList <*> hostColour

hostList :: Parser [HostAtom]
hostList = pure f <*> keyword "empty"
       <|> pure (:) <*> value <*> maybeSome (keyword ":" |> value)
  where f "empty" = []


hostColour :: Parser Colour
hostColour = keyword "#" |> pure col <*> label
        <|> pure Uncoloured
    where
        col c = fromJust $ lookup c hostColours

