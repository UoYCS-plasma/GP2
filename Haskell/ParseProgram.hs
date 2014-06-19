module ParseProgram where

import ParseLib
import GPSyntax
import ParseRule

testProgram :: String
testProgram = concat ["Main = rule1; rule2 rule1 (i:int; a:atom) ",
                      "[ (n1 (R), empty), (n2, i:a) | (e1, n1, n2, \"abc\")]",
                      "=> [ (n1, b), (n2 (R), 'q') | (e1, n1, n2, a) ]",
                      "interface = {n1, n2}",
                      "where and indeg (n1) = 2 i > 5",
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
