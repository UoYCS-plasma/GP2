module ParseRule where

import Data.Maybe

import ParseLib
import GPSyntax


-- The interface is not used: it is valid GP 2 syntax, but it is
-- not used in this implementation. Hence the parser throws away the
-- interface.
rule :: Parser AstRule
rule = pure AstRule 
       <*> lowerIdent 
       <*> (pure concat <*> maybeOne parameters)
       <*> ruleGraphs 
       <*> interface
       |> (pure head <*> exactlyOne ( keyword "where" |> condition ) <|> pure NoCondition)

-- In a rule parameter declaration, multiple variables can be declared
-- with a single type. 
parameters :: Parser [Variable]
parameters = keyword "(" 
          |> pure (++) <*> varList <*> (pure concat <*> maybeSome (keyword ";" |> varList))
          <| keyword ")" 

varList :: Parser [Variable]
varList = pure (\(ids,gptype) -> [(id,gptype) | id <- ids])
      <*> ( pure (,)
          <*> ( pure (:) <*> lowerIdent <*> maybeSome ( keyword "," |> lowerIdent ) ) 
          <| keyword ":" <*> gpType )
          
gpType :: Parser VarType
gpType = pure gptype <*> label
   where gptype t = fromJust $ lookup t gpTypes

ruleGraphs :: Parser (AstRuleGraph, AstRuleGraph)
ruleGraphs = pure (,) <*> ruleGraph <*> ( keyword "=>" |> ruleGraph )

ruleGraph :: Parser AstRuleGraph
ruleGraph = keyword "[" |> pure AstRuleGraph <*> nodeList <*> edgeList <| keyword "]"

-- Consumes the interface text: its output is discarded so the return type
-- is irrelevant.
interface :: Parser String
interface = keyword "interface" |> keyword "=" |> keyword "{" 
         |> lowerIdent <| maybeSome interfaceNodes
         <| keyword "}"

interfaceNodes :: Parser NodeName
interfaceNodes = keyword "," |> lowerIdent

nodeList :: Parser [RuleNode]
nodeList = pure (++) <*> maybeOne node <*> maybeSome (keyword "," |> node)
-- A node is a triple (Node ID, Root Node, Node Label)
-- The second component is "(R)" if root node, [] otherwise.
node :: Parser RuleNode
node = keyword "(" |> pure RuleNode 
  <*> lowerIdent
  <*> (root <| keyword ",") 
  <*> gpLabel <| keyword ")"

edgeList :: Parser [RuleEdge]
edgeList = keyword "|" |> ( pure (++) <*> maybeOne edge <*> maybeSome (keyword "," |> edge) )

                                                                           
edge :: Parser RuleEdge
edge = keyword "(" |> pure RuleEdge 
   <| lowerIdent
   <*> (bidirectional <| keyword ",")
   <*> (lowerIdent <| keyword ",") 
   <*> (lowerIdent <| keyword ",") 
   <*> (gpLabel <| keyword ")")

bidirectional :: Parser Bool
bidirectional = pure (not . null) <*> (maybeOne $ keyword "(B)")

gpLabel :: Parser RuleLabel
gpLabel = pure RuleLabel <*> list <*> ruleColour

list :: Parser GPList
list = pure f <*> keyword "empty" <|> pure (:) <*> atom <*> maybeSome (keyword ":" |> atom)
  where f "empty" = []

-- Variable rule assigns a "temporary" ListVar to each rule to conform with the
-- Haskell types. The variables are assigned their appropriate types during
-- semantic analysis.
atom :: Parser RuleAtom
atom = pure Var <*> (pure (,) <*> lowerIdent <*> pure ListVar)
   <|> pure Val <*> value
   <|> keyword "indeg" |> keyword "(" |> pure Indeg <*> lowerIdent <| keyword ")"
   <|> keyword "outdeg" |> keyword "(" |> pure Indeg <*> lowerIdent <| keyword ")"
   <|> keyword "llength" |> keyword "(" |> pure Llength <*> list <| keyword ")"
   <|> keyword "slength" |> keyword "(" |> pure Slength <*> atom <| keyword ")"
   <|> keyword "~" |> pure Neg <*> atom
   <|> keyword "+" |> pure Plus <*> atom <*> atom
   <|> keyword "-" |> pure Minus <*> atom <*> atom
   <|> keyword "*" |> pure Times <*> atom <*> atom
   <|> keyword "/" |> pure Div <*> atom <*> atom
   <|> keyword "." |> pure Concat <*> atom <*> atom


ruleColour :: Parser Colour
ruleColour = keyword "#" |> pure col <*> label
     <|> pure Uncoloured
 where
     col c = case lookup c ruleColours of
             -- This should be a syntax error.
             Nothing -> Uncoloured
             Just colour -> colour 

condition :: Parser Condition
condition = keyword "int" |> pure TestInt <*> lowerIdent
        <|> keyword "char" |> pure TestChr <*> lowerIdent
        <|> keyword "str" |> pure TestStr <*> lowerIdent
        <|> keyword "atom" |> pure TestAtom <*> lowerIdent
        <|> keyword "edge" |> keyword "(" |> 
            pure Edge <*> (lowerIdent <| keyword ",") 
                      <*> lowerIdent 
                      <*> (pure Just <*> (keyword "," |> gpLabel) <| keyword ")"
                           <|> keyword ")" |> pure Nothing )
        <|> pure Eq <*> list <| keyword "=" <*> list
        <|> pure NEq <*> list <| keyword "!=" <*> list
        <|> pure Greater <*> atom <| keyword ">" <*> atom
        <|> pure GreaterEq <*> atom <| keyword ">=" <*> atom
        <|> pure Less <*> atom <| keyword "<" <*> atom
        <|> pure LessEq <*> atom <| keyword "<=" <*> atom
        <|> keyword "not" |> pure Not <*> condition
        <|> keyword "or"  |> pure Or  <*> condition <*> condition
        <|> keyword "and" |> pure And <*> condition <*> condition 







