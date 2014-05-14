module ParseGraph where

import Data.Maybe

import ParseLib
import GP2Graph

gpNumChars = ['0'..'9']
gpChars = concat [ ['A'..'Z'] , ['a'..'z'] , gpNumChars , ['_'] ]
gpColours = [
    ("uncoloured", Uncoloured),
    ("red", Red),
    ("green", Green),
    ("blue", Blue), 
    ("grey", Grey),
    ("dashed", Dashed) ]




--gpGraph :: Parser GP2Graph
--gpGraph = keyword "[" |> gpHostGraph <| keyword "]"

--gpNodeList :: Parser GP2Graph
--gpHostGraph = gpNodeList <*> gpEdgeList

gpNodeList = atLeastOne gpNode

gpEdgeList = keyword "|" |> maybeSome gpEdge

gpNode = keyword "(" |> nodeBody <| keyword ")"

nodeBody = pure GP2Label <*> nodeValue <*> nodeColour

intOrStr  = pure Int <*> intLit
        <|> pure Str <*> strLit

nodeValue = pure (:[]) <*> intOrStr
        <|> pure (:) <*> intOrStr 

nodeColour = keyword "#" |> pure col <*> label
        <|> pure Uncoloured
    where
        col c = fromJust $ lookup c gpColours

gpEdge = pure ""

gpChar :: Parser Char
gpChar = satisfy (`elem` gpChars)

numChar :: Parser Char
numChar = satisfy (`elem` gpNumChars)

charLit :: Parser String
charLit = char '\'' |> exactlyOne gpChar <| char '\'' <| optSpaces

strLit :: Parser String
strLit = char '"' |> maybeSome gpChar <| char '"'

intLit :: Parser Int
intLit = pure read <*> atLeastOne numChar <| optSpaces

label :: Parser String
label = token ( atLeastOne gpChar )

root = keyword "(R)"

empty = keyword "empty"

--colour :: Parser [String]
--colour = satisfy (`elem` gpColours)

