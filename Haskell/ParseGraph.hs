module ParseGraph where

import Data.Maybe

import ParseLib
import GPSyntax


testCase = "(n1, 2 # blue) (n2, \"3\" # red) (n3, 'x')"
testEdge = "| (e1, n1, n2, \"cheese\" # red )"

-- gpHostGraph :: Parser GPHostGraph
-- gpHostGraph = keyword "[" |> gpHostNodeList <*> gpHostEdgeList <| keyword "]"

gpHostGraph str = 
    where
        ( str', nodes ) = gpHostNodeList str
        edges = snd $ gpHostEdgeList str'


gpHostNodeList :: Parser [(String, String, GPHostLabel)]
gpHostNodeList = atLeastOne gpHostNode

-- Old rule does not keep NodeID
-- gpNode :: Parser GP2HostLabel
-- gpNode = keyword "(" |> nodeBody <| keyword ")"

-- A node is a triple (Node ID, Root Node, Node Label)
-- The second component is "(R)" if root node, [] otherwise.
gpHostNode :: Parser (String, String, GPHostLabel)
gpHostNode = keyword "(" |> pure (,,) <*> (label <| keyword ",") <*>
             (pure concat <*> maybeOne root) <*> 
             gpHostLabel <| keyword ")"

gpHostEdgeList :: Parser [((String, String), GPHostLabel)]
gpHostEdgeList = keyword "|" |> maybeSome gpHostEdge

gpHostEdge :: Parser ((String, String), GPHostLabel)
gpHostEdge = keyword "(" |> pure (,) <*> endPoints <*> gpHostLabel <| keyword ")"

gpHostLabel :: Parser GPHostLabel
gpHostLabel = pure GPHostLabel <*> (pure (:) <*> value <*> maybeSome gpHostAtom) <*> hostColour

-- TODO: this allows leading ":" char, which is not permitted by GP2 syntax!
-- gpHostAtom :: Parser HostAtom
-- gpHostAtom = value 
--    <|> keyword ":" |> value 
-- Leading ":" issue solved below (?)
-- This generates ': value' so value <*> maybeSome gpHostAtom above should
-- parse strings of the form value [: value]
gpHostAtom :: Parser HostAtom
gpHostAtom = keyword ":" |> value

-- This feels like an awful hack, but otherwise I do not know how
-- to write a parser that generates the empty list upon reading
-- the string "empty".
gpHostList :: Parser GPHostList
gpHostList = pure f <*> empty <|> maybeSome gpHostAtom
  where f "empty" = []


hostColour :: Parser Colour
hostColour = keyword "#" |> pure col <*> label
        <|> pure Uncoloured
    where
        col c = fromJust $ lookup c gpHostColours

--colour :: Parser [String]
--colour = satisfy (`elem` gpColours)

