module ParseProgram where

import ParseLib
import GPSyntax
import ParseRule


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

localDeclaration :: Parser [LocalDecl]
localDeclaration = atLeastOne (pure LocalRule <*> rule <|> pure LocalProcedure <*> procedure)

commandSequence :: Parser CommandSequence
commandSequence = pure ComSeq <*> (pure (:) <*> command <*> maybeSome (keyword ";" |> command))

command :: Parser Command
command = pure Block <*> block
      <|> pure IfThen <*> keyword "if" |> block <*> keyword "then" |> block
      <|> pure IfThenElse <*> keyword "if" |> block <*> keyword "then" |> block <*> keyword "else" |> block
      <|> pure Try <*> keyword "try" |> block 
      <|> pure TryThen <*> keyword "try" |> block <*> keyword "then" |> block 
      <|> pure TryElse <*> keyword "try" |> block <*> keyword "else" |> block
      <|> pure TryThenElse <*> keyword "try" |> block <*> keyword "then" |> block <*> keyword "else" |> block


block :: Parser Block
block = pure LoopedComSeq <*> keyword "(" |> commandSequence <| keyword ")" <| keyword "!"
    <|> pure SimpleCommand <*> simpleCommand
    <|> pure ProgramOr <*> block <| keyword "or" <*> block

simpleCommand :: Parser SimpleCommand
simpleCommand = pure LoopedRuleCall <*> lowerIdent <| keyword "!"
            <|> pure RuleCall <*> lowerIdent
            <|> pure LoopedRuleSetCall <*> ruleSetCall <| keyword "!"
            <|> pure RuleSetCall <*> ruleSetCall
            <|> pure LoopedProcedureCall <*> upperIdent <| keyword "!"
            <|> pure ProcedureCall <*> upperIdent
            <|> pure SkipStatement <| keyword "skip"
            <|> pure FailStatement <| keyword "fail"                 

ruleSetCall :: Parser [String]
ruleSetCall = keyword "{" |> pure (:) <*> lowerIdent <*> maybeSome (keyword "," |> lowerIdent) <| keyword "}" 
