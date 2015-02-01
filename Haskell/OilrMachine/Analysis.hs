module OilrMachine.Analysis (RuleCharacterisation, characteriseRule, nodeInterfaceElem, edgeInterfaceElem, modifies, edgeClassesFor) where

import OilrMachine.Instructions
import GPSyntax
import Data.List

type Interface = [String]

data Characteristic = Unused | MinMax Int Int

-- (indeg, outdeg, loopdeg)
type NodeDegreeClassification = (Deg, Deg, Deg)

data RuleCharacterisation = RuleCharacterisation {
        rooted :: Bool, modifies :: Bool,
        nodeInterface :: Interface,
        edgeInterface :: Interface,
        structure :: [(NodeName, NodeDegreeClassification)]}


nodeInterfaceElem :: NodeName -> RuleCharacterisation -> Bool
nodeInterfaceElem n rc = n `elem` nodeInterface rc

edgeInterfaceElem :: EdgeName -> RuleCharacterisation -> Bool
edgeInterfaceElem e rc = e `elem` edgeInterface rc


edgeClassesFor :: RuleCharacterisation -> NodeName -> NodeDegreeClassification
edgeClassesFor rc id = case lookup id $ structure rc of
    Just ndc -> ndc
    Nothing  -> error $ "Couldn't find node " ++ id
        

characteriseRule :: AstRule -> RuleCharacterisation
characteriseRule r@(AstRule _ _ (lhs, rhs) _) = RuleCharacterisation root mod nif eif struc
    where
        root = areRootsUsed r
        (nif, eif, mod) = computeInterfaces lhs rhs
        struc = buildGraphStructure lhs


-- Compute the interfaces
-- TODO: oversimplification alert! Currently works by node- and edge-ids. Work can be
-- avoided by identifying nodes and edges which are preserved in a smarter way
computeInterfaces :: AstRuleGraph -> AstRuleGraph -> (Interface, Interface, Bool)
computeInterfaces (AstRuleGraph lns les) (AstRuleGraph rns res) = (nif, eif, modified)
    where
        lnids = map extractNodeName lns
        rnids = map extractNodeName rns
        extractNodeName :: RuleNode -> RuleName
        extractNodeName (RuleNode id _ _) = id
        leids = map extractEdgeName les
        reids = map extractEdgeName res
        extractEdgeName :: AstRuleEdge -> EdgeName
        extractEdgeName (AstRuleEdge id _ _ _ _) = id
        nif = intersect lnids rnids
        eif = intersect leids reids
        modified = length lns /= length nif || length les /= length eif




areRootsUsed :: AstRule -> Bool
areRootsUsed (AstRule _ _ (lhs, rhs) _ ) = hasRoot lhs || hasRoot rhs
    where
        hasRoot :: AstRuleGraph -> Bool
        hasRoot (AstRuleGraph ns _) = any (\(RuleNode _ r _) -> r) ns



-- Classify edges as in out or loop
data EdgeType = InEdge | OutEdge | LoopEdge | UninterestingEdge deriving Eq

buildGraphStructure :: AstRuleGraph -> [(NodeName, NodeDegreeClassification)]
buildGraphStructure (AstRuleGraph ns es) = [ (id, ndc) | RuleNode id _ _ <- ns , let ndc = classifyEdgesForNode id es]

classifyEdgeForId :: NodeName -> AstRuleEdge -> EdgeType
classifyEdgeForId id (AstRuleEdge _ _ i o _) =
    case (i==id, o==id) of
        (True, True) -> LoopEdge
        (True, _)    -> OutEdge
        (_, True)    -> InEdge
        _            -> UninterestingEdge

classifyEdgesForNode :: NodeName -> [AstRuleEdge] -> NodeDegreeClassification
classifyEdgesForNode id es = (outs, ins, loops)
    where
        ins   = length $ filter (==InEdge) cs
        outs  = length $ filter (==OutEdge) cs
        loops = length $ filter (==LoopEdge) cs
        cs = map (classifyEdgeForId id) es
