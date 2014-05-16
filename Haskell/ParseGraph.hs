module ParseGraph where

import Data.Maybe

import ParseLib
import GP2Graph

gpNumChars, gpChars :: [Char]
gpNumChars = ['0'..'9']
gpChars = concat [ ['A'..'Z'] , ['a'..'z'] , gpNumChars , ['_'] ]

gpColours :: [ (String, Colour) ]
gpColours = [
    ("uncoloured", Uncoloured),
    ("red", Red),
    ("green", Green),
    ("blue", Blue), 
    ("grey", Grey),
--    ("cyan", Cyan),
    ("dashed", Dashed) ]


testCase = "(n1, 2 # blue) (n2, \"3\" # red) (n3, 'x')"
testEdge = "| (e1, n1, n2, \"cheese\" # red )"

--gpGraph :: Parser GP2HostGraph
--gpGraph = keyword "[" |> gpHostGraph <| keyword "]"

--gpNodeList :: Parser GP2HostGraph
--gpHostGraph = gpNodeList <*> gpEdgeList



gpEdgeList :: Parser [((String, String), GP2HostLabel)]
gpEdgeList = keyword "|" |> maybeSome gpEdge




value :: Parser HostAtom
value = intLit
    <|> strLit
    <|> charLit

-- TODO: this allows leading ":" char, which is not permitted by GP2 syntax!
nodeValue :: Parser HostAtom
nodeValue = value 
    <|> keyword ":" |> value 

gpEdge :: Parser ((String, String), GP2HostLabel)
gpEdge = keyword "(" |> pure (,) <*> endPoints <*> gp2HostLabel <| keyword ")"


endPoints :: Parser (String, String)
endPoints = label |> keyword "," |> pure (,) <*> ( label <| keyword "," ) <*> label

gpNodeList :: Parser [GP2HostLabel]
gpNodeList = atLeastOne gpNode

gpNode :: Parser GP2HostLabel
gpNode = keyword "(" |> nodeBody <| keyword ")"

nodeBody :: Parser GP2HostLabel
nodeBody = pure GP2HostLabel <*> maybeSome nodeValue <*> nodeColour

gp2HostLabel :: Parser GP2HostLabel
gp2HostLabel = pure GP2HostLabel <*> maybeSome nodeValue <*> nodeColour


nodeColour :: Parser Colour
nodeColour = keyword "#" |> pure col <*> label
        <|> pure Uncoloured
    where
        col c = fromJust $ lookup c gpColours

numChar :: Parser Char
numChar = satisfy (`elem` gpNumChars)

charLit :: Parser HostAtom
charLit = char '\'' |> pure Chr <*> gpChar <| char '\'' <| optSpaces

strLit :: Parser HostAtom
strLit = char '"' |> pure Str <*> maybeSome gpChar <| keyword "\""
    <|>  char '\'' |> pure Str <*> exactlyOne gpChar <| keyword "'"

gpChar :: Parser Char
gpChar = satisfy (`elem` gpChars)

intLit :: Parser HostAtom
intLit = pure Int <*> ( pure read <*> atLeastOne numChar <| optSpaces )

label :: Parser String
label = token ( atLeastOne gpChar ) <| optSpaces

root :: Parser String
root = keyword "(R)"

empty :: Parser String
empty = keyword "empty"



--colour :: Parser [String]
--colour = satisfy (`elem` gpColours)

