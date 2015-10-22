module ParseProgram where

import Data.Char (toLower, isDigit)
import Data.Maybe (fromJust, isJust)
import Control.Monad (guard)
import Text.Parsec
import Text.Parsec.Expr
import ParseGraph
import GPSyntax
import Mapping

import Debug.Trace

program :: Parser GPProgram
program  =  do { ds <- many1Commented declaration ; return $ Program ds }

declaration :: Parser Declaration
declaration  =  do { gpMain }
           <|>  do { procedure }
           <|>  do { r <- rule      ; return $ AstRuleDecl r }

gpMain :: Parser Declaration
gpMain  =  do { keyword "Main" ; symbol "=" ; cs <- exprSequence ; return $ Main cs }

procedure :: Parser Declaration
procedure  =  do { id <- upperIdent ; symbol "=" ;
                   ds <- option [] $
                           between (symbol "[") (symbol "]") (many1 localDeclaration) ;
                   cs <- exprSequence ; return $ Proc id ds cs }

localDeclaration :: Parser Declaration
localDeclaration  =  do { r <- rule ; return $ AstRuleDecl r }
                <|>  procedure

{-
data Expr = IfStatement Expr Expr Expr
          | TryStatement Expr Expr Expr
          | Looped Expr
          | Sequence [Expr]
          | Set [Expr]
          | ProcedureCall ProcName
          | RuleCall RuleName
          | Skip
          | Fail
          deriving (Show, Eq)
          -}
exprSequence :: Parser [Expr]
exprSequence = sepBy1 expr (symbol ";")

expr :: Parser Expr
expr  =  do { keyword "if"  ; condExpr IfStatement }
     <|> do { keyword "try" ; condExpr TryStatement }
     <|> do { cs <- between (symbol "(") (symbol ")") exprSequence ;
              option (Sequence cs) $ do { symbol "!" ; return $ Looped (Sequence cs) } }
     <|> do { id <- upperIdent; option (ProcedureCall id) $
                                do { symbol "!" ; return $ Looped $ ProcedureCall id } }
     <|> do { rs <- ruleSet; option (RuleSet rs) $
                             do { symbol "!" ; return $ Looped $ RuleSet rs } }
                                 
     <|> do { keyword "skip" ; return Skip }
     <|> do { keyword "fail" ; return Fail }

condExpr :: (Expr -> Expr -> Expr -> Expr) -> Parser Expr
condExpr constr = do { c <- expr ;
                       t <- option Skip $ do { keyword "then" ; expr } ;
                       e <- option Skip $ do { keyword "else" ; expr } ;
                       case c of
                       IfStatement _ _ _  -> error "Use brackets to nest if/try blocks"
                       TryStatement _ _ _ -> error "Use brackets to nest try/if blocks"
                       _                  -> return $ constr c t e }

ruleSet :: Parser [String]
ruleSet  =  between (symbol "{") (symbol "}") (sepBy1 lowerIdent (symbol ","))
       <|>  do { r <- lowerIdent ; return [r] }


{-
commandSequence :: Parser [Command] 
commandSequence  =  sepBy1 command (symbol ";")

command :: Parser Command
command  =  do { keyword "if" ; c <- block ;
                 t <- option skip $ do { keyword "then" ; block } ;
                 e <- option skip $ do { keyword "else" ; block } ;
                 return $ IfStatement c t e }
       <|>  do { keyword "try" ; c <- block ;
                 t <- option skip $ do { keyword "then" ; block } ;
                 e <- option skip $ do { keyword "else" ; block } ;
                 return $ TryStatement c t e }
       <|>  do { b <- block ; return $ Block b }
  where skip  =  SimpleCommand Skip

block :: Parser Block
block  =  do { keyword "or" ; b1 <- block ; b2 <- block ; return $ ProgramOr b1 b2 }
     <|>  do { symbol "(" ; cs <- commandSequence ; symbol ")" ;
               option (ComSeq cs) $ do { symbol "!" ; return $ LoopedComSeq cs } }
     <|>  do { sc <- simpleCommand ; return $ SimpleCommand sc }

simpleCommand :: Parser SimpleCommand
simpleCommand  =  do { keyword "skip" ; return Skip }
             <|>  do { keyword "fail" ; return Fail }
             <|>  do { id <- upperIdent ; 
                       option (ProcedureCall id) $
                              do { symbol "!" ; return $ LoopedProcedureCall id } }
             <|>  do { rs <- ruleSetCall ; 
                       option (RuleCall rs) $
                              do { symbol "!" ; return $ LoopedRuleCall rs } }
-} 
rule :: Parser AstRule
rule  =  do { id <- lowerIdent ; symbol "(" ; ps <- parameters ; symbol ")" ;
              lhs <- ruleGraph ; symbol "=>" ; rhs <- ruleGraph ; interface ;
              c <- option NoCondition $ do { keyword "where" ; condition } ;
              return $ AstRule id ps (lhs,rhs) c }

parameters :: Parser [Variable]
parameters  =  do { vss <- sepBy varList (symbol ";") ; return $ concat vss }

varList :: Parser [Variable]
varList  =  do { ids <- sepBy1 lowerIdent (symbol ",") ; symbol ":" ; t <- gpType ;
                 return [(id,t) | id <- ids] }
          
gpType :: Parser VarType
gpType  =  do { t <- many1 lower ; let { vt = lookup t gpTypes } ;
                guard $ isJust vt ; return $ fromJust vt }

ruleGraph :: Parser AstRuleGraph
ruleGraph  =  do { symbol "[" ; ns <- manyCommented node ;
                   symbol "|" ; es <- manyCommented edge ;
                   symbol "]" ; return $ AstRuleGraph ns es }

-- An interface is parsed, but no information about it is
-- currently stored in the abstract syntax.
interface :: Parser ()
interface  =  do { keyword "interface" ; symbol "=" ;
                   ids <- between (symbol "{") (symbol "}") $
                            sepBy lowerIdent (symbol ",") ; return () }

node :: Parser RuleNode
node  =  do { symbol "(" ; id <- lowerIdent ; r <- root ; symbol "," ; l <- gpLabel ;
              symbol ")" ; return $ RuleNode id r l }

edge :: Parser AstRuleEdge
edge  =  do { symbol "(" ; id <- lowerIdent ; b <- bidirectional ; symbol "," ;
              s <- lowerIdent ; symbol "," ; t <- lowerIdent ; symbol "," ;
              l <- gpLabel ;
              symbol ")" ; return $ AstRuleEdge id b s t l }

bidirectional :: Parser Bool
bidirectional  =  option False $ do { symbol "(B)" ; return True }

gpLabel :: Parser RuleLabel
gpLabel  =  do { as <- list ; rc <- ruleColour ; return $ RuleLabel as rc }

list :: Parser GPList
list  =  do { keyword "empty" ; return [] } <|> sepBy1 atom (symbol ":")

atom :: Parser RuleAtom
atom = buildExpressionParser atomops atom'

atomops = [ [ Prefix (do { symbol "-"; return Neg }) ],
            [ Infix  (do { symbol "."; return Concat }) AssocLeft ],
            [ Infix  (do { symbol "*"; return Times }) AssocLeft, 
              Infix  (do { symbol "/"; return Div }) AssocLeft ],
            [ Infix  (do { symbol "+"; return Plus }) AssocLeft,
              Infix  (do { symbol "~"; return Minus }) AssocLeft ] ]

-- Var atoms are temporarily assigned the type ListVar.  Actual
-- types are determined later.
atom' :: Parser RuleAtom
atom' =  do { symbol "(" ; a <- atom ; symbol ")" ; return a }
    <|>  do { keyword "indeg"   ; symbol "(" ; id <- lowerIdent ;
                                  symbol ")" ; return $ Indeg id }
    <|>  do { keyword "outdeg"  ; symbol "(" ; id <- lowerIdent ;
                                  symbol ")" ; return $ Outdeg id }
    <|>  do { keyword "llength" ; symbol "(" ; l <- list ;
                                  symbol ")" ; return $ Llength l }
    <|>  do { keyword "slength" ; symbol "(" ; a <- atom ;
                                  symbol ")" ; return $ Slength a }
    <|>  do { v <- value ; return $ Val v }
    <|>  do { id <- lowerIdent ; return $ Var (id,ListVar) }

ruleColour :: Parser Colour
ruleColour  =  do { symbol "#" ; c <- many1 lower ; spaces ;
                    let { rc = lookup c ruleColours } ;
                    guard $ isJust rc ; return $ fromJust rc }
          <|>  return Uncoloured

condition :: Parser Condition
condition = buildExpressionParser boolops predicate

boolops = [ [ Prefix (do { symbol "not" ; return Not }) ],
            [ Infix  (do { symbol "and" ; return And }) AssocLeft ],
            [ Infix  (do { symbol "or"  ; return Or }) AssocLeft ] ]

predicate :: Parser Condition
predicate =  do { symbol "(" ; c <- condition ; symbol ")" ; return c }
        <|>  do { keyword "int"  ; id <- lowerIdent ; return $ TestInt id }
        <|>  do { keyword "char" ; id <- lowerIdent ; return $ TestChr id }
        <|>  do { keyword "str"  ; id <- lowerIdent ; return $ TestStr id }
        <|>  do { keyword "atom" ; id <- lowerIdent ; return $ TestAtom id }
        <|>  do { keyword "edge" ; symbol "(" ;
                  s <- lowerIdent ; symbol "," ; t <- lowerIdent ;
                  ml <- option Nothing $
                               do { symbol "," ; l <- gpLabel ; return $ Just l } ;
                  symbol ")" ; return $ Edge s t ml }
        <|>  ( try $ do { a1 <- atom ;
                          op <- (    do { symbol ">"  ; return Greater }
                                <|>  do { symbol ">=" ; return GreaterEq }
                                <|>  do { symbol "<"  ; return Less }
                                <|>  do { symbol "<=" ; return LessEq } ) ;
                          a2 <- atom ; return $ op a1 a2 } )
        <|>  ( try $ do { l1 <- list ;
                          op <- (    do { symbol "=" ;  return Eq }
                                <|>  do { symbol "!=" ; return NEq } ) ;
                          l2 <- list ; return $ op l1 l2 } )

upperIdent :: Parser String
upperIdent  =  do { id <- identifier upper ; spaces; return id }

