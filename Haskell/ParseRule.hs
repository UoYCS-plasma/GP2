module ParseRule where

import Data.Maybe

import ParseLib
import GPSyntax

--ruleDecl :: Parser RuleDecl
--ruleDecl = lowerIdent <| keyword "(" <*>  


--gpGraph :: Parser GPRuleGraph
--gpGraph = keyword "[" |> gpNodeList <*> gpEdgeList <| keyword "]"

gpNodeList :: Parser [(String, String, GPLabel)]
gpNodeList = atLeastOne gpNode

-- A node is a triple (Node ID, Root Node, Node Label)
-- The second component is "(R)" if root node, [] otherwise.
gpNode :: Parser (String, String, GPLabel)
gpNode = keyword "(" |> pure (,,) <*> (label <| keyword ",") <*> (pure concat <*> maybeOne root) <*> gpLabel <| keyword ")"

gpEdgeList :: Parser [((String, String), GPLabel)]
gpEdgeList = keyword "|" |> maybeSome gpEdge

gpEdge :: Parser ((String, String), GPLabel)
gpEdge = keyword "(" |> pure (,) <*> endPoints <*> gpLabel <| keyword ")"


gpLabel :: Parser GPLabel
gpLabel = pure GPLabel <*> gpList <*> ruleColour

-- This feels like an awful hack, but otherwise I do not know how
-- to write a parser that generates the empty list upon reading
-- the string "empty".
gpList :: Parser GPList
gpList = pure f <*> empty <|> atLeastOne gpAtom
  where f "empty" = []

gpAtom :: Parser Atom
gpAtom = atom
     <|> keyword ":" |> atom
	 

atom :: Parser Atom
atom = pure Var <*> lowerIdent
   <|> pure Val <*> value
   <|> keyword "indeg" |> keyword "(" |> pure Indeg <*> lowerIdent <| keyword ")"
   <|> keyword "outdeg" |> keyword "(" |> pure Indeg <*> lowerIdent <| keyword ")"
   <|> keyword "llength" |> keyword "(" |> pure Llength <*> gpList <| keyword ")"
   <|> keyword "slength" |> keyword "(" |> pure Slength <*> gpList <| keyword ")"
   <|> keyword "-" |> pure Neg <*> atom
   <|> keyword "+" |> pure Plus <*> atom <*> atom
   <|> keyword "-" |> pure Minus <*> atom <*> atom
   <|> keyword "*" |> pure Times <*> atom <*> atom
   <|> keyword "/" |> pure Div <*> atom <*> atom
   <|> keyword "." |> pure Concat <*> atom <*> atom


ruleColour :: Parser Colour
ruleColour = keyword "#" |> pure col <*> label
     <|> pure Uncoloured
 where
     col c = fromJust $ lookup c gpColours

-- edge rule not working because of the maybeOne.
condition :: Parser Condition
condition = keyword "int" |> pure TestInt <*> lowerIdent
        <|> keyword "char" |> pure TestStr <*> lowerIdent
        <|> keyword "str" |> pure TestChar <*> lowerIdent
        <|> keyword "atom" |> pure TestAtom <*> lowerIdent
        <|> keyword "edge" |> keyword "(" |> pure Edge <*> (lowerIdent <| keyword ",") <*> lowerIdent <*> (pure concat <*> maybeOne (keyword "," |> gpLabel))
        <|> pure Eq <*> gpList <| keyword "=" |> gpList
	<|> pure NEq <*> gpList <| keyword "!=" |> gpList
        <|> pure Greater <*> atom <| keyword ">" |> atom
        <|> pure GreaterEq <*> atom <| keyword ">=" |> atom
        <|> pure Less <*> atom <| keyword "<" |> atom
        <|> pure LessEq <*> atom <| keyword "<=" |> atom
        <|> keyword "not" <| pure Not <*> condition
        <|> pure Or <*> condition <| keyword "or" |> condition
        <|> pure And <*> condition <| keyword "and" |> condition 







