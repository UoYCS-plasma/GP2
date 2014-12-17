module ProcessAst where

import Prelude 
import Data.List
import Data.Maybe
import GPSyntax
import Graph
import Mapping

-- A symbol's Scope is the procedure it is contained in. Global symbols
-- have scope "Global".
-- RuleId is used to distinguish between variables, nodes and edges who may
-- have the same names in different rules. The field is the empty string
-- for rule and procedure symbols.
type Scope = String
type RuleID = String
type SymbolTable = Mapping String Symbol
data Symbol = Symbol VarType Scope RuleID

lookupSymbols :: SymbolTable -> String -> [Symbol]
lookupSymbols symbols id = [symbol | (name, symbol) <- symbols, name == id]

-- symbolInScope takes an identifier <id>, a scope ("Global" or a procedure
-- name), a rule name and a symbol table. It returns the symbol with
-- name <id> with the same Scope and RuleID as those passed to the function. 
-- Assumes there is at most one symbol in said scope, as variable names
-- cannot be repeated within a single GP 2 rule.
symbolInScope :: VarName -> Scope -> RuleID -> SymbolTable -> Maybe Symbol
symbolInScope name scope rule table = find (checkScope scope rule) $ lookupSymbols table name 
  where 
  -- checkScope :: String -> String -> Symbol -> Bool
     checkScope scope rule (Symbol _ symbolScope symbolRule) = 
        scope == symbolScope && rule == symbolRule


-- Transforms the GP program AST into a data structure suitable for rule application.
-- The only transformations are performed on rule declarations: the rule's graphs are
-- converted to our graph ADT, and any variable names occurring in the rule are
-- assigned their correct type according to the rule's variable list. The information
-- from the variable list is stored in the SymbolTable which is created by the call
-- to makeSymbolTable.
makeGPProgram :: GPProgram -> (GPProgram, SymbolTable)
makeGPProgram (Program decls) = (Program $ map (makeDeclaration "Global" table) decls, table)
  where table = makeSymbolTable "Global" [] decls 

makeDeclaration :: Scope -> SymbolTable -> Declaration -> Declaration
makeDeclaration s t (AstRuleDecl r) = RuleDecl $ makeRule r s t
makeDeclaration _ _ d = d

-- Uses makeSymbolTable' to scan the program for rule declarations and to enter 
-- the variables into the symbol table.
makeSymbolTable :: Scope -> SymbolTable -> [Declaration] -> SymbolTable
makeSymbolTable scope table decls  = foldl' (makeSymbolTable' scope) table decls

makeSymbolTable' :: Scope -> SymbolTable -> Declaration -> SymbolTable
makeSymbolTable' scope table decl = case decl of
  MainDecl _ -> table
  ProcDecl _ -> table 
  AstRuleDecl (AstRule id vars _ _) -> enterVariables scope id table vars
 
enterVariables :: Scope -> RuleID -> SymbolTable -> [Variable] -> SymbolTable
enterVariables scope rule table vars = foldl' (enterVariable scope rule) table vars 

enterVariable :: Scope -> RuleID -> SymbolTable -> Variable -> SymbolTable
enterVariable scope rule table (id, gpType) = addItem table id (Symbol gpType scope rule)

-- NodeMap and EdgeMap keeps track of the correspondences between string IDs in 
-- the AstGraphs and the integer node-id and edge-ids in graph data structures.
-- Only bidirectional edges are placed in the mapping.
type NodeMap = Mapping NodeName NodeId
type EdgeMap = Mapping EdgeName EdgeId

-- makeHostGraph first generates all the nodes so that the node map is available
-- for edge creation. It uses the node map to ensure that the edges are assigned
-- the correct source and target nodes.
makeHostGraph :: AstHostGraph -> HostGraph
makeHostGraph (AstHostGraph hns hes) = fst $ foldr addHEdge (nodeGraph,nodeMaps) hes
    where (nodeGraph, nodeMaps) = foldr addHNode (emptyGraph, []) hns

addHNode :: HostNode -> (HostGraph, NodeMap) -> (HostGraph, NodeMap)
addHNode hn@(HostNode id _ _ ) (g, nm) = (g', (id, newId):nm)
    where (g', newId) = newNode g hn

-- Uses the node map to lookup the appropriate node IDs from the HostEdge's
-- source and target nodes, and creates the appropriate edge in the HostGraph.
-- The source and target node of each edge are expected to be in the graph, so
-- the lookups should never return Nothing.
addHEdge :: HostEdge -> (HostGraph, NodeMap) -> (HostGraph, NodeMap)
addHEdge (HostEdge src tgt label) (g, nm) = 
    let srcId = lookup src nm
        tgtId = lookup tgt nm in
    case (srcId, tgtId) of
        (Nothing, Nothing) -> error $ "Edge source " ++ show src ++ " and target " ++ show tgt ++ " undefined."
        (Nothing, _) -> error $ "Edge source " ++ show src ++ " undefined."
        (_, Nothing) -> error $ "Edge target " ++ show tgt ++ " undefined."
        (Just srcId, Just tgtId) -> (fst $ newEdge g srcId tgtId label, nm)


makeRule :: AstRule -> Scope -> SymbolTable -> Rule
makeRule (AstRule name vars (lhs, rhs) cond) scope table =
         Rule name vars (lhs', rhs') nInterface' eInterface' cond'
    where
        (lhs', lnm, lem) = makeRuleGraph lhs scope name table 
        (rhs', rnm, rem) = makeRuleGraph rhs scope name table
        nInterface' = makeNodeInterface lnm rnm
        eInterface' = makeEdgeInterface lem rem
        cond' = makeCondition cond scope name table 

-- makeRuleGraph has the same structure as makeHostGraph with respect to
-- generating the graph. Some additional processing is required to assign
-- variables (in labels) their correct types according to the symbol table.
-- The functions updateNode and updateEdge make any necessary amendments
-- to node labels and edge labels respectively. The function assignTypes
-- is responsible for the bulk of the work.
-- Furthermore, there is an EdgeMap which contains the mappings for any
-- bidirectional edges.

makeRuleGraph :: AstRuleGraph -> Scope -> RuleID -> SymbolTable -> (RuleGraph, NodeMap, EdgeMap)
makeRuleGraph (AstRuleGraph rns res) scope rule table = foldr addREdge (nodeGraph, nodeMaps, []) res'
    where (nodeGraph, nodeMaps) = foldr addRNode (emptyGraph, []) rns'
          rns' = map (updateNode scope rule table) rns
          res' = map (updateEdge scope rule table) res

addRNode :: RuleNode -> (RuleGraph, NodeMap) -> (RuleGraph, NodeMap)
addRNode hn@(RuleNode id _ _ ) (g, nm) = (g', (id, newId):nm)
    where (g', newId) = newNode g hn

addREdge :: AstRuleEdge -> (RuleGraph, NodeMap, EdgeMap) -> (RuleGraph, NodeMap, EdgeMap)
addREdge (AstRuleEdge id bidir src tgt label) (g, nm, em) = 
    let srcId = lookup src nm
        tgtId = lookup tgt nm in
    case (srcId, tgtId) of
        (Nothing, Nothing) -> error $ "Edge source " ++ show src ++ " and target " ++ show tgt ++ " undefined."
        (Nothing, _) -> error $ "Edge source " ++ show src ++ " undefined."
        (_, Nothing) -> error $ "Edge target " ++ show tgt ++ " undefined."
        (Just srcId, Just tgtId) -> (g', nm, if bidir then ((id, newId):em) else em)
            where (g', newId) = newEdge g srcId tgtId (RuleEdge id bidir label)

updateNode :: Scope -> RuleID -> SymbolTable -> RuleNode -> RuleNode
updateNode scope rule table (RuleNode id isRoot ( RuleLabel list mark ) ) =
    let newList = assignTypes scope rule table list 
    in (RuleNode id isRoot ( RuleLabel newList mark ) ) 
      

updateEdge :: Scope -> RuleID -> SymbolTable -> AstRuleEdge -> AstRuleEdge
updateEdge scope rule table (AstRuleEdge id bidir src tgt ( RuleLabel list mark ) ) =
    let newList = assignTypes scope rule table list
    in (AstRuleEdge id bidir src tgt ( RuleLabel newList mark ) ) 

-- Creates the interface from the NodeMaps generated by calls to makeRuleGraph on
-- the LHS and RHS. It finds all instances of NodeNames that occur in both
-- NodeMaps and, for each one, creates a pair of the LeftNodeId and RightNodeId
-- according to both NodeMaps.
makeNodeInterface :: NodeMap -> NodeMap -> NodeInterface
makeNodeInterface lnm rnm = [ (definiteLookup name lnm, definiteLookup name rnm) | name <- interfaceNames ]
    where
        interfaceNames = map fst lnm `intersect` map fst rnm

makeEdgeInterface :: EdgeMap -> EdgeMap -> EdgeInterface
makeEdgeInterface lem rem = [ (definiteLookup name lem, definiteLookup name rem) | name <- interfaceNames ]
    where
        interfaceNames = map fst lem `intersect` map fst rem

-- Finds variables in the condition and assigns them their correct types
-- according to the symbol table.
makeCondition :: Condition -> Scope -> RuleID -> SymbolTable -> Condition 
makeCondition c scope rule table = case c of
    Greater a1 a2 -> Greater (assignTypes' scope rule table a1) (assignTypes' scope rule table a2)
    GreaterEq a1 a2 -> GreaterEq (assignTypes' scope rule table a1) (assignTypes' scope rule table a2)
    Less a1 a2 -> Less (assignTypes' scope rule table a1) (assignTypes' scope rule table a2)
    LessEq a1 a2 -> LessEq (assignTypes' scope rule table a1) (assignTypes' scope rule table a2)
    Not c -> Not $ makeCondition c scope rule table  
    Or c1 c2 -> Or (makeCondition c1 scope rule table) (makeCondition c2 scope rule table)
    And c1 c2 -> And (makeCondition c1 scope rule table) (makeCondition c2 scope rule table)
    c -> c

-- Assigns variables in rule labels their correct VarTypes from the symbol 
-- table.
-- The function traverses the GPList: if a Var is encountered, symbolInScope
-- and getType are used to extract the correct type from the appropriate
-- symbol. 
assignTypes :: Scope -> RuleID -> SymbolTable -> GPList -> GPList
assignTypes scope rule table as = map (assignTypes' scope rule table) as

assignTypes' :: Scope -> RuleID -> SymbolTable -> RuleAtom -> RuleAtom
assignTypes' scope rule table a = case a of
    Var (name, gpType) -> Var (name, newType) 
        where newType = getType name $ symbolInScope name scope rule table
    Slength a -> Slength (assignTypes' scope rule table a)
    Plus a b -> Plus (assignTypes' scope rule table a) (assignTypes' scope rule table b)
    Minus a b -> Minus (assignTypes' scope rule table a) (assignTypes' scope rule table b)
    Times a b -> Times (assignTypes' scope rule table a) (assignTypes' scope rule table b)
    Div a b -> Div (assignTypes' scope rule table a) (assignTypes' scope rule table b)
    Concat a b -> Concat (assignTypes' scope rule table a) (assignTypes' scope rule table b)
    a -> a

getType :: String -> Maybe Symbol -> VarType
getType name Nothing             = error $ "Variable " ++ name ++ " not found in the rule." 
getType _ (Just (Symbol gpType _ _)) = gpType



