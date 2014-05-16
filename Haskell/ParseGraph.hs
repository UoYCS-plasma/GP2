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

--gpGraph :: Parser GP2Graph
--gpGraph = keyword "[" |> gpHostGraph <| keyword "]"

--gpNodeList :: Parser GP2Graph
--gpHostGraph = gpNodeList <*> gpEdgeList



gpEdgeList :: Parser [((String, String), GP2Label)]
gpEdgeList = keyword "|" |> maybeSome gpEdge


gpEdge :: Parser ((String, String), GP2Label)
gpEdge = keyword "(" |> pure (,) <*> endPoints <*> gp2Label <| keyword ")"


endPoints :: Parser (String, String)
endPoints = label |> keyword "," |> pure (,) <*> ( label <| keyword "," ) <*> label

gpNodeList :: Parser [GP2Label]
gpNodeList = atLeastOne gpNode

gpNode :: Parser GP2Label
gpNode = keyword "(" |> nodeBody <| keyword ")"

nodeBody :: Parser GP2Label
nodeBody = label |> keyword "," |> gp2Label

gp2Label :: Parser GP2Label
gp2Label = pure GP2Label <*> maybeSome nodeValue <*> nodeColour

nodeColour :: Parser Colour
nodeColour = keyword "#" |> pure col <*> label
        <|> pure Uncoloured
    where
        col c = fromJust $ lookup c gpColours

-- TODO: this allows leading ":" char, which is not permitted by GP2 syntax!
nodeValue :: Parser IntOrStr
nodeValue = intOrStr 
    <|> keyword ":" |> intOrStr 



intOrStr :: Parser IntOrStr
intOrStr  = intLit
        <|> strLit

numChar :: Parser Char
numChar = satisfy (`elem` gpNumChars)

charLit :: Parser String
charLit = char '\'' |> exactlyOne gpChar <| char '\'' <| optSpaces

strLit :: Parser IntOrStr
strLit = char '"' |> pure Str <*> maybeSome gpChar <| keyword "\""
    <|>  char '\'' |> pure Str <*> exactlyOne gpChar <| keyword "'"

gpChar :: Parser Char
gpChar = satisfy (`elem` gpChars)

intLit :: Parser IntOrStr
intLit = pure Int <*> ( pure read <*> atLeastOne numChar <| optSpaces )

label :: Parser String
label = token ( atLeastOne gpChar ) <| optSpaces

root :: Parser String
root = keyword "(R)"

empty :: Parser String
empty = keyword "empty"



--colour :: Parser [String]
--colour = satisfy (`elem` gpColours)

