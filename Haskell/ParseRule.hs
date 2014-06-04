module ParseRule where

import Data.Maybe

import ParseLib
import GPSyntax

ruleDecl :: Parser Rule
ruleDecl = pure Rule 
       <*> lowerIdent 
       <*> keyword "(" |> (pure (:) <*> varList <*> maybeSome (keyword ";" |> varList)) <| keyword ")" 
       <*> ruleGraphs
       <*> interface
       <*> condition 
       <*> keyword "injective" |> keyword "=" |> (keyword "true" <|> keyword "false")
           

-- In a rule parameter declaration, multiple variables can be declared
-- with a single type. The type is represented as a String.
varList :: Parser Variables
varList = pure (,) <*> atLeastOne lowerIdent <| keyword ":" <*> gpType  

gpType :: Parser String
gpType = keyword "int" <|> keyword "char" <|> keyword "string" <|>
         keyword "atom" <|> keyword "list"


ruleGraphs :: Parser (RuleGraph, RuleGraph)
ruleGraphs = pure (,) <*> ruleGraph <*> keyword "=>" |> ruleGraph

interface :: Parser Interface
interface = keyword "interface" |> keyword "=" |> keyword "{" 
         |> pure (:) <*> lowerIdent <*> maybeSome interfaceNodes 
         <| keyword "}"

interfaceNodes :: Parser ID
interfaceNodes = keyword "," |> lowerIdent 

ruleGraph :: Parser RuleGraph
ruleGraph = keyword "[" |> pure RuleGraph <*> nodeList <*> edgeList <| keyword "]"

nodeList :: Parser [RuleNode]
nodeList = atLeastOne node

-- A node is a triple (Node ID, Root Node, Node Label)
-- The second component is "(R)" if root node, [] otherwise.
node :: Parser RuleNode
node = keyword "(" |> pure RuleNode 
  <*> (label <| keyword ",") 
  <*> (pure concat <*> maybeOne root) 
  <*> gpLabel <| keyword ")"

edgeList :: Parser [RuleEdge]
edgeList = keyword "|" |> maybeSome edge

edge :: Parser RuleEdge
edge = keyword "(" |> pure RuleEdge 
   <*> (lowerIdent <| keyword ",") 
   <*> (lowerIdent <| keyword ",") 
   <*> (gpLabel <| keyword ")")

gpLabel :: Parser RuleLabel
gpLabel = pure RuleLabel <*> list <*> ruleColour


list :: Parser GPList
list = pure f <*> keyword "empty" <|> pure (:) <*> atom <*> maybeSome (keyword ":" |> atom)
  where f "empty" = []

atom :: Parser RuleAtom
atom = pure Var <*> lowerIdent
   <|> pure Val <*> value
   <|> keyword "indeg" |> keyword "(" |> pure Indeg <*> lowerIdent <| keyword ")"
   <|> keyword "outdeg" |> keyword "(" |> pure Indeg <*> lowerIdent <| keyword ")"
   <|> keyword "llength" |> keyword "(" |> pure Llength <*> list <| keyword ")"
   <|> keyword "slength" |> keyword "(" |> pure Slength <*> list <| keyword ")"
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
     col c = fromJust $ lookup c ruleColours

condition :: Parser Condition
condition = keyword "int" |> pure TestInt <*> lowerIdent
        <|> keyword "char" |> pure TestStr <*> lowerIdent
        <|> keyword "str" |> pure TestChar <*> lowerIdent
        <|> keyword "atom" |> pure TestAtom <*> lowerIdent
        <|> keyword "edge" |> keyword "(" |> 
            pure Edge <*> (lowerIdent <| keyword ",") 
                      <*> lowerIdent 
                      <*> (pure head <*> maybeOne (keyword "," |> gpLabel))
        <|> pure Eq <*> list <| keyword "=" <*> list
	<|> pure NEq <*> list <| keyword "!=" <*> list
        <|> pure Greater <*> atom <| keyword ">" <*> atom
        <|> pure GreaterEq <*> atom <| keyword ">=" <*> atom
        <|> pure Less <*> atom <| keyword "<" <*> atom
        <|> pure LessEq <*> atom <| keyword "<=" <*> atom
        <|> keyword "not" |> pure Not <*> condition
        <|> pure Or <*> condition <| keyword "or" <*> condition
        <|> pure And <*> condition <| keyword "and" <*> condition 







