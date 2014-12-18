module ParseGraph where

import Data.Char (toLower, isDigit)
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

value :: Parser HostAtom
value = intLit <|> strLit <|> charLit

intLit :: Parser HostAtom
intLit = pure (Int . read) <*> atLeastOne (satisfy isDigit) <| optSpaces

charLit :: Parser HostAtom
charLit = char '\'' |> pure Chr <*> gpChar <| char '\'' <| optSpaces

strLit :: Parser HostAtom
strLit = char '"' |> pure Str <*> maybeSome gpChar <| keyword "\""
    <|>  char '\'' |> pure Str <*> exactlyOne gpChar <| keyword "'"

gpChar :: Parser Char
gpChar = satisfy (`elem` gpChars)

identifier :: Parser Char -> Parser String
identifier first = guarded g (pure (:) <*> first <*> maybeSome gpChar)
  where g s = (map toLower s) `notElem` keywords

lowerIdent :: Parser String
lowerIdent = identifier lower <| optSpaces

label :: Parser String
label = token ( atLeastOne gpChar ) <| optSpaces

root :: Parser Bool
root = pure (not . null) <*> (maybeOne $ keyword "(R)")


