module ParseProgram where

import ParseLib
import GPSyntax
import ParseRule

testProgram :: String
testProgram = concat ["Main = rule1 rule1 (i:int; l:list) ",
                      "[ (n1, i), (n2, l) | (e1, n1, n2, empty)]",
                      "=> [ (n1, i), (n2, l:i) | (e1, n1, n2, empty) ]",
                      "interface = {n1, n2}",
                      "where edge (n1,n2)",
                      "injective = true"]

program :: Parser GPProgram
program = pure Program <*> atLeastOne declaration 

declaration :: Parser Declaration
declaration = pure MainDecl <*> main
          <|> pure ProcDecl <*> procedure
          <|> pure RuleDecl <*> rule

main :: Parser Main
main = keyword "Main" |> keyword "=" |> pure Main <*> commandSequence

procedure :: Parser Procedure
procedure = pure Procedure 
        <*> upperIdent 
        <*> keyword "=" |> (pure concat <*> maybeOne (keyword "[" |> localDeclaration <| keyword "]"))
        <*> commandSequence

localDeclaration :: Parser [Declaration]
localDeclaration = atLeastOne (pure RuleDecl <*> rule <|> pure ProcDecl <*> procedure)

commandSequence :: Parser CommandSequence
commandSequence = pure Sequence <*> (pure (:) <*> command <*> maybeSome (keyword ";" |> command))

command :: Parser Command
command = pure Block <*> block
      <|> keyword "if" |> pure IfStatement <*> block <*> keyword "then" |> block
          <*> (keyword "else" |> block <|> (pure $ SimpleCommand Skip))
      <|> keyword "try" |> pure TryStatement <*> (pure $ SimpleCommand Skip) 
          <*> (pure $ SimpleCommand Skip) <*> (pure $ SimpleCommand Skip)
      <|> keyword "try" |> pure TryStatement <*> block 
          <*> (keyword "then" |> block <|> (pure $ SimpleCommand Skip))
          <*> (keyword "else" |> block <|> (pure $ SimpleCommand Skip)) 

block :: Parser Block
block = pure LoopedComSeq <*> keyword "(" |> commandSequence <| keyword ")" <| keyword "!"
    <|> pure ComSeq <*> keyword "(" |> commandSequence <| keyword ")" 
    <|> pure SimpleCommand <*> simpleCommand
    <|> pure ProgramOr <*> keyword "or" |> block <*> block

simpleCommand :: Parser SimpleCommand
simpleCommand = pure LoopedRuleCall <*> lowerIdent <| keyword "!"
            <|> pure RuleCall <*> lowerIdent
            <|> pure LoopedRuleSetCall <*> ruleSetCall <| keyword "!"
            <|> pure RuleSetCall <*> ruleSetCall
            <|> pure LoopedProcedureCall <*> upperIdent <| keyword "!"
            <|> pure ProcedureCall <*> upperIdent
            <|> pure Skip <| keyword "skip"
            <|> pure Fail <| keyword "fail"                 

ruleSetCall :: Parser [String]
ruleSetCall = keyword "{" |> pure (:) <*> lowerIdent <*> maybeSome (keyword "," |> lowerIdent) <| keyword "}" 
