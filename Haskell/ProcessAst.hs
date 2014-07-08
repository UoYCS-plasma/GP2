module ProcessAst where

import Prelude 
import Data.List
import Data.Maybe
import GPSyntax
import Graph
import ExAr
import Mapping

testHg = AstHostGraph [HostNode "n1" False (HostLabel [Int 1] Uncoloured),HostNode "n2" False (HostLabel [Str "hello"] Uncoloured),HostNode "n3" False (HostLabel [Int 1,Int 1] Uncoloured)] [HostEdge "n1" "n2" (HostLabel [] Uncoloured),HostEdge "n1" "n3" (HostLabel [] Uncoloured)]

type SymbolTable = Mapping String Symbol 

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


type SymbolList = [(String, Symbol)]

lookupSymbols :: SymbolTable -> String -> [Symbol]
lookupSymbols symbols id = [symbol | (name,symbol) <- symbols, name == id]

makeTable :: SymbolList -> SymbolTable
makeTable = foldr (\(id,s) table -> addItem table id s) []

-- symbolsInScope takes an identifier <id>, a scope ("Global" or a procedure
-- name), a rule name and a symbol table. It returns the list of symbols with
-- name <id> with the same Scope and RuleID as those passed into the function. 

symbolsInScope :: VarName -> Scope -> RuleID -> SymbolTable -> [Symbol]
symbolsInScope name scope rule table = filter (checkScope scope rule) $ lookupSymbols table name 
  where 
  -- checkScope :: String -> String -> Symbol -> Bool
     checkScope scope rule (Symbol _ symbolScope symbolRule) = 
        scope == symbolScope && rule == symbolRule

makeGPProgram :: GPProgram -> (GPProgram, SymbolTable)
makeGPProgram (Program decls) = (Program $ map (makeDeclaration "Global" table) decls, table)
  where table = enterDeclarations "Global" [] decls 


makeDeclaration :: Scope -> SymbolTable -> Declaration -> Declaration
makeDeclaration s t (AstRuleDecl r) = RuleDecl r'
    where
        r' = makeRule r s t
makeDeclaration _ _ x = x

-- Calls enterDeclarations with "Global" scope and an empty symbol table.
enterSymbols :: GPProgram -> SymbolTable
enterSymbols (Program declarations) = enterDeclarations "Global" [] declarations

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
                                      in addItem table' id (Symbol Procedure_S scope "")
  AstRuleDecl (AstRule id vars _ _) -> let table' = enterVariables scope id table vars
                                      in addItem table' id (Symbol Rule_S scope "")

enterVariables :: Scope -> RuleID -> SymbolTable -> [Variable] -> SymbolTable
enterVariables s r t vars = foldl' (enterVariable s r) t vars 

enterVariable :: Scope -> RuleID -> SymbolTable -> Variable -> SymbolTable
enterVariable s r t (id,gptype) = addItem t id (Symbol (Var_S gptype False) s r)

-- NodeMap keeps track of the correspondence between string IDs in the AstGraphs
-- and the integer IDs in the ExAr graphs.
type NodeMap = Mapping NodeName NodeId

-- makeHostGraph first generates all the nodes so that the node map is available
-- for edge creation. It uses the node map to ensure that the edges are assigned
-- the correct source and target nodes.
makeHostGraph :: AstHostGraph -> HostGraph
makeHostGraph (AstHostGraph hns hes) = fst $ foldr addHEdge (nodeGraph,nodeMaps) hes
    where (nodeGraph, nodeMaps) = foldr addHNode (emptyGraph, []) hns

addHNode :: HostNode -> (HostGraph, NodeMap) -> (HostGraph, NodeMap)
addHNode hn@(HostNode id _ _ ) (g, nm) = (g', (id, newId):nm)
    where (g', newId) = newNode g hn

-- Uses the node map to lookup the appropriate node IDs from the
-- HostEdge's source and target nodes, and creates the appropriate
-- edge in the HostGraph.
-- The source and target node of each edge are expected to be in
-- the graph, so the lookups should never return Nothing.
addHEdge :: HostEdge -> (HostGraph, NodeMap) -> (HostGraph, NodeMap)
addHEdge (HostEdge src tgt label) (g, nm) = (g',nm)
    where srcId = definiteLookup src nm
          tgtId = definiteLookup tgt nm
          (g', _) = newEdge g srcId tgtId label

-- May need to keep the new SymbolTable t' but I ignore it for now.
makeRule :: AstRule -> Scope -> SymbolTable -> Rule
makeRule (AstRule name vars (lhs, rhs) cond) s t =
         Rule name vars (lhs', rhs') interface' cond
    where
        t' = enterVariables s name t vars
        (lhs',lnm) = makeRuleGraph lhs s name t' 
        (rhs',rnm) = makeRuleGraph rhs s name t'
        interface' = makeInterface lnm rnm

-- Creates the interface from the NodeMaps generated by calls to makeRuleGraph on
-- the LHS and RHS. It finds all instances of NodeNames that occur in both
-- NodeMaps and, for each one, creates a pair of the LeftNodeId and RightNodeId
-- according to both NodeMaps.
makeInterface :: NodeMap -> NodeMap -> Interface
makeInterface lnm rnm = [ (definiteLookup name lnm, definiteLookup name rnm) | name <- interfaceNames ]
    where
        interfaceNames = map fst lnm `intersect` map fst rnm
    
-- makeRuleGraph has the same structure as makeHostGraph with respect to
-- generating the graph. Some additional processing is required to assign
-- variables (in labels) their correct types according to the symbol table.
-- The functions updateNode and updateEdge make any necessary amendments
-- to node labels and edge labels respectively. The function assignTypes
-- is responsible for the bulk of the work.

makeRuleGraph :: AstRuleGraph -> Scope -> RuleID -> SymbolTable -> (RuleGraph, NodeMap)
makeRuleGraph (AstRuleGraph rns res) s r t = foldr addREdge (nodeGraph,nodeMaps) res'
    where (nodeGraph,nodeMaps) = foldr addRNode (emptyGraph, []) rns'
          rns' = map (updateNode s r t) rns
          res' = map (updateEdge s r t) res

addRNode :: RuleNode -> (RuleGraph, NodeMap) -> (RuleGraph, NodeMap)
addRNode hn@(RuleNode id _ _ ) (g, nm) = (g', (id, newId):nm)
    where (g', newId) = newNode g hn

addREdge :: RuleEdge -> (RuleGraph, NodeMap) -> (RuleGraph, NodeMap)
addREdge (RuleEdge _ src tgt label) (g, nm) = (g',nm)
    where Just srcId = lookup src nm
          Just tgtId = lookup tgt nm
          (g', _) = newEdge g srcId tgtId label


updateNode :: Scope -> RuleID -> SymbolTable -> RuleNode -> RuleNode
updateNode s r t (RuleNode id b ( RuleLabel list c ) ) =
    let newList = assignTypes list s r t 
    in (RuleNode id b ( RuleLabel newList c ) ) 
      

updateEdge :: Scope -> RuleID -> SymbolTable -> RuleEdge -> RuleEdge
updateEdge s r t (RuleEdge b src tgt ( RuleLabel list c ) ) =
    let newList = assignTypes list s r t
    in (RuleEdge b src tgt ( RuleLabel newList c ) ) 


-- Assigns variables in rule labels their correct VarTypes from the symbol 
-- table.
-- The function traverses the GPList: if a Var is encountered, symbolsInScope
-- and getType are used to extract the correct type from the appropriate
-- symbol. 

assignTypes :: GPList -> Scope -> RuleID -> SymbolTable -> GPList
assignTypes [] _ _ _                        = []
assignTypes ((Var (name, gpType)):as) s r t = 
    let newType = getType $ symbolsInScope name s r t in
    case newType of 
        -- If Nothing, no variable was found in the rule. Semantic error.
        Nothing -> Var ("poo", ListVar) : assignTypes as s r t
        Just gpType  -> Var (name, gpType) : assignTypes as s r t 
assignTypes (a:as) s r t                    = a : assignTypes as s r t 

getType :: [Symbol] -> Maybe VarType
getType []                             = Nothing 
getType (Symbol ( Var_S t _ ) _ _ :ss) = Just t
getType (s:ss)                         = getType ss



