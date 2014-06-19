module SemanticAnalysis where

import Prelude hiding (lookup)
import Data.List  
import GPSyntax
import ExAr
import Graph

testProg :: GPProgram
testProg = Program 
   [MainDecl (Main 
      (Sequence 
        [Block (SimpleCommand (RuleCall "rule1")),
         Block (SimpleCommand (RuleCall "rule2"))]
      )
    ),
   RuleDecl (Rule "rule1" 
       [("i",IntVar),("a",AtomVar)] 
       (AstRuleGraph 
          [RuleNode "n1" True (RuleLabel [] Uncoloured),
           RuleNode "n2" False (RuleLabel [Var ("i",ListVar),Var ("a",ListVar)] Uncoloured)] 
          [RuleEdge False "n1" "n2" (RuleLabel [Val (Str "abc")] Uncoloured)],
       AstRuleGraph
          [RuleNode "n1" False (RuleLabel [Var ("b",ListVar)] Uncoloured),
           RuleNode "n2" True (RuleLabel [Val (Str "q")] Uncoloured)] 
          [RuleEdge False "n1" "n2" (RuleLabel [Var ("a",ListVar)] Uncoloured)])
       ["n1","n2"] 
       (And (Eq [Indeg "n1"] [Val (Int 2)]) (Greater (Var ("i",ListVar)) (Val (Int 5)))) "true"
    ),
   ProcDecl (Procedure "proc1" 
       [RuleDecl (Rule "rule1"
        [("a",AtomVar),("a",AtomVar)]
        (AstRuleGraph [] [], AstRuleGraph [] [])
        ["n5","n5"]
        (And (TestInt "a") (NEq ([Var ("a",ListVar)]) ([Var ("x",ListVar)])))
        "true")
       ]
       (Sequence [Block (SimpleCommand (RuleCall "rule3"))])
    )
   ]


-- My idea is to use ExAr indexed by Strings (names of symbols)
-- This requires changing the ExAr data structure to take two type parameters
-- e.g. ExAr a b = ExAr [(a,b)] Int
-- and define data Graph = Graph (ExAr Int (Node a)) (ExAr Int (Edge b))
type SymbolTable = ExAr String Symbol 

type Scope = String
type RuleID = String

-- A symbol's Scope is the procedure it is contained in. Global symbols
-- have scope "Global".
-- RuleId is used to distinguish between variables, nodes and edges who may
-- have the same names in different rules. The field is the empty string
-- for rule and procedure symbols.
data Symbol = Symbol SymbolType Scope RuleID deriving (Show)

-- A variable symbol Var_S is equipped with its type VarType and a Bool set to
-- True if the variable occurs in the LHS of a rule.
-- A node symbol's Bool is True if the node is a wildcard.
-- An edge symbol's Bool is True if the edge is bidirectional.
-- Edge's IDs are discarded in parsing. Perhaps their "IDs" can be a concatenation
-- of the source and target IDs?
data SymbolType = Procedure_S
                | Rule_S
                | Var_S VarType Bool
                | LeftNode_S Bool
                | RightNode_S Bool
                | LeftEdge_S Bool
                | RightEdge_S Bool
   deriving (Show)


type SymbolList = [(ID, Symbol)]

makeTable :: SymbolList -> SymbolTable
makeTable = foldr (\(id,s) table -> addSymbol table id s) empty

testtab = makeTable slist

slist = [("a", Symbol (Var_S AtomVar False) "Global" "r1"),
         ("l", Symbol (Var_S ListVar False) "Global" "r1"),
         ("i", Symbol (Var_S IntVar False) "Global" "r1"),
         ("i", Symbol (Var_S IntVar False) "Global" "r2"),
         ("i", Symbol (Var_S ChrVar False) "Lol" "r1")]

-- symbolsInScope takes an identifier <id>, a scope ("Global" or a procedure
-- name), a rule name and a symbol table. It returns the list of symbols with
-- name <id> with the same Scope and RuleID as those passed into the function. 

symbolsInScope :: ID -> Scope -> RuleID -> SymbolTable -> [Symbol]
symbolsInScope name scope rule table = filter (checkScope scope rule) $ listLookup table name 
  where 
  -- checkScope :: String -> String -> Symbol -> Bool
     checkScope scope rule (Symbol _ symbolScope symbolRule) = 
        scope == symbolScope && rule == symbolRule



-- Calls enterDeclarations with "Global" scope and an empty symbol table.
enterSymbols :: GPProgram -> SymbolTable
enterSymbols (Program declarations) = enterDeclarations "Global" empty declarations

-- Enter any rule and procedure declarations into the symbol table.
enterDeclarations :: Scope -> SymbolTable -> [Declaration] -> SymbolTable
enterDeclarations scope table decls  = foldl' (enterDeclarations' scope) table decls

-- MainDecl: contains only a command sequence, no declarations to enter.
-- ProcDecl: the name of the procedure being declared is entered into the symbol
--           table and the local declaration list of the procedure is processed
--           by a recursive call.
-- RuleDecl: the name of the rule and its variables are entered into the symbol table.
enterDeclarations' :: Scope -> SymbolTable -> Declaration -> SymbolTable
enterDeclarations' scope table decl = case decl of
  MainDecl _ -> table
  ProcDecl (Procedure id decls _ ) -> let table' = enterDeclarations id table decls 
                                      in addSymbol table' id (Symbol Procedure_S scope "")
  RuleDecl (Rule id vars _ _ _ _ ) -> let table' = enterVariables scope id table vars
                                      in addSymbol table' id (Symbol Rule_S scope "")


enterVariables :: Scope -> RuleID -> SymbolTable -> [Variable] -> SymbolTable
enterVariables s r t vars = foldl' (enterVariable s r) t vars 

enterVariable :: Scope -> RuleID -> SymbolTable -> Variable -> SymbolTable
enterVariable s r t (id,gptype) = addSymbol t id (Symbol (Var_S gptype False) s r)


