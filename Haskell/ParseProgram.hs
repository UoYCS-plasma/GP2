module ParseProgram where

import Data.Char (toLower, isDigit)
import Data.Maybe (fromJust, isJust)
import ParseLib
import ParseGraph
import GPSyntax
import Mapping

program :: Parser GPProgram
program = optSpaces |> pure Program <*> atLeastOne declaration 

declaration :: Parser Declaration
declaration = pure MainDecl <*> gpMain
          <|> pure ProcDecl <*> procedure
          <|> pure AstRuleDecl <*> rule

gpMain :: Parser Main
gpMain = keyword "Main" |> keyword "=" |> pure Main <*> commandSequence

procedure :: Parser Procedure
procedure = pure Procedure 
        <*> upperIdent 
        <*> keyword "=" |> (pure concat <*> maybeOne (keyword "[" |> localDeclaration <| keyword "]"))
        <*> commandSequence

localDeclaration :: Parser [Declaration]
localDeclaration = atLeastOne (pure AstRuleDecl <*> rule <|> pure ProcDecl <*> procedure)

commandSequence :: Parser [Command] 
commandSequence = atLeastOneSep command (keyword ";") 

command :: Parser Command
command = pure Block <*> block
      <|> keyword "if" |> pure IfStatement <*> block <*> keyword "then" |> block
          <*> (keyword "else" |> block <|> (pure $ SimpleCommand Skip))
      <|> keyword "try" |> pure TryStatement <*> block 
          <*> (keyword "then" |> block <|> (pure $ SimpleCommand Skip))
          <*> (keyword "else" |> block <|> (pure $ SimpleCommand Skip))
      <|> keyword "try" |> pure TryStatement <*> (pure $ SimpleCommand Skip) 
          <*> (pure $ SimpleCommand Skip) <*> (pure $ SimpleCommand Skip) 

block :: Parser Block
block = pure LoopedComSeq <*> keyword "(" |> commandSequence <| keyword ")" <| keyword "!"
    <|> pure ComSeq <*> keyword "(" |> commandSequence <| keyword ")" 
    <|> pure SimpleCommand <*> simpleCommand
    <|> pure ProgramOr <*> keyword "or" |> block <*> block

simpleCommand :: Parser SimpleCommand
simpleCommand = pure LoopedRuleCall <*> ruleSetCall <| keyword "!"
            <|> pure RuleCall <*> ruleSetCall
            <|> pure LoopedProcedureCall <*> upperIdent <| keyword "!"
            <|> pure ProcedureCall <*> upperIdent
            <|> pure Skip <| keyword "skip"
            <|> pure Fail <| keyword "fail"         

ruleSetCall :: Parser [String]
ruleSetCall = keyword "{" |> atLeastOneSep lowerIdent (keyword ",") <| keyword "}" 
          <|> pure (:[]) <*> lowerIdent

-- The interface is not used: it is valid GP 2 syntax, but it is
-- not used in this implementation. Hence the parser throws away the
-- interface.
rule :: Parser AstRule
rule = pure AstRule 
       <*> lowerIdent 
       <*> (pure concat <*> keyword "(" |> maybeOne parameters <| keyword ")" )
       <*> ruleGraphs 
       <*> interface |> (keyword "where" |> condition <|> pure NoCondition)

-- In a rule parameter declaration, multiple variables can be declared
-- with a single type. 
parameters :: Parser [Variable]
parameters = pure concat <*> atLeastOneSep varList (keyword ";")

varList :: Parser [Variable]
varList = pure (\ids gptype -> [(id,gptype) | id <- ids])
          <*> atLeastOneSep lowerIdent (keyword ",") <| keyword ":" <*> gpType
          
gpType :: Parser VarType
gpType = pure (flip definiteLookup gpTypes) <*> label

ruleGraphs :: Parser (AstRuleGraph, AstRuleGraph)
ruleGraphs = pure (,) <*> ruleGraph <*> ( keyword "=>" |> ruleGraph )

ruleGraph :: Parser AstRuleGraph
ruleGraph = keyword "[" |> pure AstRuleGraph 
        <*> maybeSome node <| keyword "|"   
        <*> maybeSome edge <| keyword "]"

-- Consumes the interface text: its output is discarded so the return type
-- is irrelevant.
interface :: Parser String
interface = keyword "interface" |> keyword "=" 
         |> keyword "{" |> pure concat <*>
              maybeOne (lowerIdent <| maybeSome (keyword "," |> lowerIdent))
         <| keyword "}"

-- A node is a triple (Node ID, Root Node, Node Label)
-- The second component is "(R)" if root node, [] otherwise.
node :: Parser RuleNode
node = keyword "(" |> pure RuleNode 
  <*> lowerIdent
  <*> (root <| keyword ",") 
  <*> gpLabel <| keyword ")"

edge :: Parser AstRuleEdge
edge = keyword "(" |> pure AstRuleEdge 
   <*> lowerIdent
   <*> (bidirectional <| keyword ",")
   <*> (lowerIdent <| keyword ",") 
   <*> (lowerIdent <| keyword ",") 
   <*> (gpLabel <| keyword ")")

bidirectional :: Parser Bool
bidirectional = pure (not . null) <*> (maybeOne $ keyword "(B)")

gpLabel :: Parser RuleLabel
gpLabel = pure RuleLabel <*> list <*> ruleColour

list :: Parser GPList
list =  pure [] <| keyword "empty" <|> atLeastOneSep atom (keyword ":")

-- Variable rule assigns a "temporary" ListVar to each rule to conform with the
-- Haskell types. The variables are assigned their appropriate types during
-- semantic analysis.
atom :: Parser RuleAtom
atom = pure Var <*> (pure (,) <*> lowerIdent <*> pure ListVar)
   <|> pure Val <*> value
   <|> keyword "indeg" |> keyword "(" |> pure Indeg <*> lowerIdent <| keyword ")"
   <|> keyword "outdeg" |> keyword "(" |> pure Outdeg <*> lowerIdent <| keyword ")"
   <|> keyword "llength" |> keyword "(" |> pure Llength <*> list <| keyword ")"
   <|> keyword "slength" |> keyword "(" |> pure Slength <*> atom <| keyword ")"
   <|> keyword "~" |> pure Neg <*> atom
   <|> keyword "+" |> pure Plus <*> atom <*> atom
   <|> keyword "-" |> pure Minus <*> atom <*> atom
   <|> keyword "*" |> pure Times <*> atom <*> atom
   <|> keyword "/" |> pure Div <*> atom <*> atom
   <|> keyword "." |> pure Concat <*> atom <*> atom

ruleColour :: Parser Colour
ruleColour = keyword "#" |> pure fromJust <*> guarded isJust
               (pure (`lookup` ruleColours) <*> label)
         <|> pure Uncoloured

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

upperIdent :: Parser String
upperIdent = identifier upper <| optSpaces

