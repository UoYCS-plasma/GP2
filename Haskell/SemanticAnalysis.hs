module SemanticAnalysis where

import GPSyntax.hs
import ExAr.hs

-- Required for some functions in the module ExAr.
instance Eq String where
  s == t = compare s t = EQ

-- My idea is to use ExAr indexed by Strings (names of symbols)
-- This requires changing the ExAr data structure to take two type parameters
-- e.g. ExAr a b = ExAr [(a,b)] Int
-- and define data Graph = Graph (ExAr Int (Node a)) (ExAr Int (Edge b))
data SymbolTable = SymbolTable (ExAr String Symbol)

type Scope = String
type ContainingRule = String



data Symbol = Symbol SymbolType Scope ContainingRule

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

-- Enter any rule and procedure declarations into the symbol table.
enterDeclarations :: GPProgram -> SymbolTable
enterDeclarations (Program decls) = case decls of 
   


