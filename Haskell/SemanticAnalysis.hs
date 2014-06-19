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

-- symbolsInScope takes an identifier <id>, a scope ("Global" or a procedure
-- name), a rule name and a symbol table. It returns the list of symbols with
-- name <id> with the same Scope and ContaininingRule as those passed into
-- the function. 

symbolsInScope :: ID -> Scope -> RuleID -> SymbolTable -> [Symbol]
symbolsInScope name scope rule table = filter (checkScope scope rule) $ listLookup table name 
  where 
  -- checkScope :: String -> String -> Symbol -> [Symbol]
     checkScope s1 s2 (Symbol _ sc r) = s1 == sc && s2 == r



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


-- To keep track of the correspondence between string IDs in the AstGraphs
-- and the integer IDs in the graphs.

type NodeMap = [(ID, NodeId)]
type SymbolList = [(ID, Symbol)]

testrg :: AstRuleGraph
testrg = AstRuleGraph 
        [RuleNode "n1" False (RuleLabel [Var ("a", ListVar), Var ("l", ListVar)] Red),
         RuleNode "n2" False (RuleLabel [Var ("i", ListVar)] Uncoloured)]
        [RuleEdge False "n1" "n2" (RuleLabel [Var ("i", ListVar)] Uncoloured)]

makeTable :: SymbolList -> SymbolTable
makeTable = foldr (\(id,s) table -> addSymbol table id s) empty

testtab = makeTable slist

slist = [("a", Symbol (Var_S AtomVar False) "Global" "r1"),
         ("l", Symbol (Var_S ListVar False) "Global" "r1"),
         ("i", Symbol (Var_S IntVar False) "Global" "r1"),
         ("i", Symbol (Var_S IntVar False) "Global" "r2"),
         ("i", Symbol (Var_S ChrVar False) "Lol" "r1")]


makeRuleGraph :: AstRuleGraph -> Scope -> RuleID -> SymbolTable -> RuleGraph
makeRuleGraph (AstRuleGraph rns res) s r t = fst $ foldr addREdge (nodeGraph,nodeMaps) res'
  where (nodeGraph,nodeMaps) = foldr addRNode (emptyGraph, []) rns'
        rns' = map (updateNode s r t) rns
        res' = map (updateEdge s r t) res

addRNode :: RuleNode -> (RuleGraph, NodeMap) -> (RuleGraph, NodeMap)
addRNode hn@(RuleNode id _ _ ) (g, nm) = (g', (id, newID):nm)
  where (g', newID) = newNode g hn

addREdge :: RuleEdge -> (RuleGraph, NodeMap) -> (RuleGraph, NodeMap)
addREdge (RuleEdge _ src tgt label) (g, nm) = (g',nm)
  where Just srcID = lookup src nm
        Just tgtID = lookup tgt nm
        (g', _) = newEdge g srcID tgtID label


updateNode :: Scope -> RuleID -> SymbolTable -> RuleNode -> RuleNode
updateNode s r t (RuleNode id b ( RuleLabel list c ) ) =
  let newList = assignTypes list s r t 
  in (RuleNode id b ( RuleLabel newList c ) ) 
      

updateEdge :: Scope -> RuleID -> SymbolTable -> RuleEdge -> RuleEdge
updateEdge s r t (RuleEdge b src tgt ( RuleLabel list c ) ) =
  let newList = assignTypes list s r t
  in (RuleEdge b src tgt ( RuleLabel newList c ) ) 


-- Assigns variables their correct VarTypes from the symbol table.
assignTypes :: GPList -> Scope -> RuleID -> SymbolTable -> GPList
assignTypes [] _ _ _                        = []
assignTypes ((Var (name, gpType)):as) s r t = 
  let Just newType = getType $ symbolsInScope name s r t 
  in Var (name, newType) : assignTypes as s r t 
assignTypes (a:as) s r t                    = a : assignTypes as s r t 

getType :: [Symbol] -> Maybe VarType
getType []                             = Nothing
getType (Symbol ( Var_S t _ ) _ _ :ss) = Just t
getType (s:ss)                         = getType ss


testhg :: AstHostGraph
testhg = AstHostGraph 
        [HostNode "n1" False (HostLabel [Int 1, Str "one"] Red),
         HostNode "n2" False (HostLabel [Chr 'a'] Uncoloured)]
        [HostEdge "n1" "n2" (HostLabel [] Uncoloured)]

makeHostGraph :: AstHostGraph -> HostGraph
makeHostGraph (AstHostGraph hns hes) = fst $ foldr addHEdge (nodeGraph,nodeMaps) hes
  where (nodeGraph,nodeMaps) = foldr addHNode (emptyGraph, []) hns

addHNode :: HostNode -> (HostGraph, NodeMap) -> (HostGraph, NodeMap)
addHNode hn@(HostNode id _ _ ) (g, nm) = (g', (id, newID):nm)
  where (g', newID) = newNode g hn

addHEdge :: HostEdge -> (HostGraph, NodeMap) -> (HostGraph, NodeMap)
addHEdge (HostEdge src tgt label) (g, nm) = (g',nm)
  where Just srcID = lookup src nm
        Just tgtID = lookup tgt nm
        (g', _) = newEdge g srcID tgtID label

