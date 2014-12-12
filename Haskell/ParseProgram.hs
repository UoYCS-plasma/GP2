module ParseProgram where

import ParseLib
import GPSyntax
import ParseRule

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
commandSequence = pure (:) <*> command <*> maybeSome (keyword ";" |> command)

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
ruleSetCall = keyword "{" |> pure (:) 
          <*> lowerIdent <*> maybeSome (keyword "," |> lowerIdent) 
           <| keyword "}" 
          <|> pure (:[]) <*> lowerIdent
