module ParseGraph where

import Data.Maybe

import ParseLib
import GPSyntax
import Graph


testCase = "(n1, 2 # blue) (n2, \"3\" # red) (n3, 'x')"
testEdge = "| (e1, n1, n2, \"cheese\" # red )"

hostGraph :: Parser AstHostGraph
hostGraph = optSpaces |> keyword "[" |> pure AstHostGraph <*> hostNodeList <*> hostEdgeList <| keyword "]"

hostNodeList :: Parser [HostNode]
hostNodeList = pure (++) <*> maybeOne hostNode <*> maybeSome (keyword "," |> hostNode)

-- A node is a triple (Node ID, Root Node, Node Label)
-- The second component is "(R)" if root node, [] otherwise.
hostNode :: Parser HostNode
hostNode = keyword "(" |> pure HostNode
       <*> label
       <*> (root <| keyword ",") 
       <*> (hostLabel <| keyword ")")

hostEdgeList :: Parser [HostEdge]
hostEdgeList = keyword "|" |> ( pure (++) <*> maybeOne hostEdge <*> maybeSome (keyword "," |> hostEdge) )


hostEdge :: Parser HostEdge
hostEdge = keyword "(" |> pure HostEdge
       <| ( (lowerIdent <| keyword ",") )
       <*> (lowerIdent <| keyword ",")
       <*> (lowerIdent <| keyword ",")
       <*> (hostLabel <| keyword ")")

hostLabel :: Parser HostLabel
hostLabel = pure HostLabel <*> hostList <*> hostColour

hostList :: Parser [HostAtom]
hostList = pure f <*> keyword "empty" <|> pure (:) <*> value <*> maybeSome (keyword ":" |> value)
  where f "empty" = []


hostColour :: Parser Colour
hostColour = keyword "#" |> pure col <*> label
        <|> pure Uncoloured
    where
        col c = fromJust $ lookup c hostColours

