module ParseRule where

import Data.Maybe

import ParseLib
import GPSyntax

--ruleDecl :: Parser RuleDecl
--ruleDecl = lowerIdent <*> keyword "(" |> maybeSome varList <| keyword ")" <*> graphs 
--           <*> interface <*> maybeOne condition <*> keyword "injective" |> keyword "=" 
--           |> keyword "true" <|> keyword "false"
           

-- In a rule parameter declaration, multiple variables can be declared
-- with a single type. The type is represented as a String.
varList :: Parser ([Variable], String)
varList = pure (,) <*> atLeastOne lowerIdent <| keyword ":" <*> gptype <| maybeOne (keyword ";")

gptype :: Parser String
gptype = keyword "int" <|> keyword "char" <|> keyword "string" <|>
         keyword "atom" <|> keyword "list"


--graphs :: Parser (Graph, Graph)
--graphs = gpGraph <*> keyword "=>" |> gpGraph

interface :: Parser [Id]
interface = keyword "interface" |> keyword "=" |> keyword "{" 
         |> pure (:) <*> lowerIdent <*> maybeSome interfaceNodes 
         <| keyword "}"

interfaceNodes :: Parser Id
interfaceNodes = keyword "," |> lowerIdent 

--gpGraph :: Parser GPRuleGraph
--gpGraph = keyword "[" |> gpNodeList <*> gpEdgeList <| keyword "]"

gpNodeList :: Parser [(String, String, RuleLabel)]
gpNodeList = atLeastOne gpNode

-- A node is a triple (Node ID, Root Node, Node Label)
-- The second component is "(R)" if root node, [] otherwise.
gpNode :: Parser (String, String, RuleLabel)
gpNode = keyword "(" |> pure (,,) <*> (label <| keyword ",") <*> (pure concat <*> maybeOne root) <*> gpLabel <| keyword ")"

gpEdgeList :: Parser [((String, String), RuleLabel)]
gpEdgeList = keyword "|" |> maybeSome gpEdge

gpEdge :: Parser ((String, String), RuleLabel)
gpEdge = keyword "(" |> pure (,) <*> endPoints <*> gpLabel <| keyword ")"


gpLabel :: Parser RuleLabel
gpLabel = pure RuleLabel <*> gpList <*> ruleColour

-- This feels like an awful hack, but otherwise I do not know how
-- to write a parser that generates the empty list upon reading
-- the string "empty".
gpList :: Parser [RuleAtom]
gpList = pure f <*> empty <|> atLeastOne gpAtom
  where f "empty" = []

gpAtom :: Parser RuleAtom
gpAtom = atom
     <|> keyword ":" |> atom
	 

atom :: Parser RuleAtom
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
     col c = fromJust $ lookup c gpRuleColours

-- edge rule not working because of the maybeOne.
condition :: Parser Condition
condition = keyword "int" |> pure TestInt <*> lowerIdent
        <|> keyword "char" |> pure TestStr <*> lowerIdent
        <|> keyword "str" |> pure TestChar <*> lowerIdent
        <|> keyword "atom" |> pure TestAtom <*> lowerIdent
        <|> keyword "edge" |> keyword "(" |> pure Edge 
            <*> (lowerIdent <| keyword ",") <*> lowerIdent 
            <*> (pure head <*> maybeOne (keyword "," |> gpLabel))
        <|> pure Eq <*> gpList <| keyword "=" <*> gpList
	<|> pure NEq <*> gpList <| keyword "!=" <*> gpList
        <|> pure Greater <*> atom <| keyword ">" <*> atom
        <|> pure GreaterEq <*> atom <| keyword ">=" <*> atom
        <|> pure Less <*> atom <| keyword "<" <*> atom
        <|> pure LessEq <*> atom <| keyword "<=" <*> atom
        <|> keyword "not" |> pure Not <*> condition
        <|> pure Or <*> condition <| keyword "or" <*> condition
        <|> pure And <*> condition <| keyword "and" <*> condition 







