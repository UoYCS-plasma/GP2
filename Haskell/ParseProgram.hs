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

-- Bracketed command sequences. To be used whenever bracketing is required,
-- such as in conditional branching statements.
block :: Parser [Command]
block = keyword "(" |> commandSequence <| keyword ")"

command :: Parser Command
command = pure Conditional <*> conditional
      <|> pure ProgramOr <*> keyword "or" |> block <*> block
      <|> pure LoopedRuleCall <*> ruleSetCall <| keyword "!"
      <|> pure RuleCall <*> ruleSetCall
      <|> pure LoopedProcedureCall <*> upperIdent <| keyword "!"
      <|> pure ProcedureCall <*> upperIdent
      <|> pure Loop <*> block <| keyword "!"
      <|> pure Skip <| keyword "skip"
      <|> pure Fail <| keyword "fail"                 

conditional :: Parser Conditional
conditional = keyword "if" |> pure IfStatement <*> block <*> keyword "then" |> block
              <*> (keyword "else" |> block <|> (pure (:[]) <*> pure Skip))
          <|> keyword "try" |> pure TryStatement <*> block 
              <*> (keyword "then" |> block <|> (pure (:[]) <*> pure Skip))
              <*> (keyword "else" |> block <|> (pure (:[]) <*> pure Skip)) 
          <|> keyword "try" |> pure TryStatement <*> (pure (:[]) <*> pure Skip) 
              <*> (pure (:[]) <*> pure Skip) <*> (pure (:[]) <*> pure Skip)

ruleSetCall :: Parser [String]
ruleSetCall = keyword "{" |> pure (:) 
          <*> lowerIdent <*> maybeSome (keyword "," |> lowerIdent) 
           <| keyword "}" 
          <|> pure (:[]) <*> lowerIdent
