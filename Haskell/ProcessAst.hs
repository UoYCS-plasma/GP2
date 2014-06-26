module ProcessAst where

import Prelude 
import Data.List
import GPSyntax
import SemanticAnalysis
import Graph
import ExAr

testrg :: AstRuleGraph
testrg = AstRuleGraph 
        [RuleNode "n1" False (RuleLabel [Var ("a", ListVar), Var ("l", ListVar)] Red),
         RuleNode "n2" False (RuleLabel [Var ("i", ListVar)] Uncoloured)]
        [RuleEdge False "n1" "n2" (RuleLabel [Var ("i", ListVar)] Uncoloured)]

testhg :: AstHostGraph
testhg = AstHostGraph 
        [HostNode "n1" False (HostLabel [Int 1, Str "one"] Red),
         HostNode "n2" False (HostLabel [Chr 'a'] Uncoloured)]
        [HostEdge "n1" "n2" (HostLabel [] Uncoloured)]


-- NodeMap keeps track of the correspondence between string IDs in the AstGraphs
-- and the integer IDs in the ExAr graphs.

type NodeMap = [(NodeName, NodeId)]

-- makeHostGraph first generates all the nodes so that the node map is available
-- for edge creation. It uses the node map to ensure that the edges are assigned
-- the correct source and target nodes.
makeHostGraph :: AstHostGraph -> HostGraph
makeHostGraph (AstHostGraph hns hes) = fst $ foldr addHEdge (nodeGraph,nodeMaps) hes
  where (nodeGraph,nodeMaps) = foldr addHNode (emptyGraph, []) hns

addHNode :: HostNode -> (HostGraph, NodeMap) -> (HostGraph, NodeMap)
addHNode hn@(HostNode id _ _ ) (g, nm) = (g', (id, newId):nm)
  where (g', newId) = newNode g id hn

-- Uses the node map to lookup the appropriate node IDs from the
-- HostEdge's source and target nodes, and creates the appropriate
-- edge in the HostGraph.
-- The source and target node of each edge are expected to be in
-- the graph, so the lookups should never return Nothing.
addHEdge :: HostEdge -> (HostGraph, NodeMap) -> (HostGraph, NodeMap)
addHEdge (HostEdge src tgt label) (g, nm) = (g',nm)
  where Just srcId = lookup src nm
        Just tgtId = lookup tgt nm
        (g', _) = newEdge g (src ++ tgt) srcId tgtId label


-- makeRuleGraph has the same structure as makeHostGraph with respect to
-- generating the graph. Some additional processing is required to assign
-- variables (in labels) their correct types according to the symbol table.
-- The functions updateNode and updateEdge make any necessary amendments
-- to node labels and edge labels respectively. The function assignTypes
-- is responsible for the bulk of the work.

makeRuleGraph :: AstRuleGraph -> Scope -> RuleID -> SymbolTable -> RuleGraph
makeRuleGraph (AstRuleGraph rns res) s r t = fst $ foldr addREdge (nodeGraph,nodeMaps) res'
  where (nodeGraph,nodeMaps) = foldr addRNode (emptyGraph, []) rns'
        rns' = map (updateNode s r t) rns
        res' = map (updateEdge s r t) res

addRNode :: RuleNode -> (RuleGraph, NodeMap) -> (RuleGraph, NodeMap)
addRNode hn@(RuleNode id _ _ ) (g, nm) = (g', (id, newId):nm)
  where (g', newId) = newNode g id hn

addREdge :: RuleEdge -> (RuleGraph, NodeMap) -> (RuleGraph, NodeMap)
addREdge (RuleEdge _ src tgt label) (g, nm) = (g',nm)
  where Just srcId = lookup src nm
        Just tgtId = lookup tgt nm
        (g', _) = newEdge g (src ++ tgt) srcId tgtId label


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
assignTypes ((Var (name, gpType)):as) s r t = Var (name, newType) : assignTypes as s r t 
  where Just newType = getType $ symbolsInScope name s r t 
assignTypes (a:as) s r t                    = a : assignTypes as s r t 

getType :: [Symbol] -> Maybe VarType
getType []                             = error "No variable symbol in scope."
getType (Symbol ( Var_S t _ ) _ _ :ss) = Just t
getType (s:ss)                         = getType ss



