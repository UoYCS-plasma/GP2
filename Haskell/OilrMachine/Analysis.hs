module OilrMachine.Analysis (RuleCharacterisation, characteriseRule, nodeInterfaceElem, edgeInterfaceElem, modifies, edgeClassesFor) where

import OilrMachine.Instructions
import GPSyntax
import Data.List
import Graph

-- (indeg, outdeg, loopdeg)
type NodeDegreeClassification = (Deg, Deg, Deg)

data RuleCharacterisation = RuleCharacterisation {
        rooted :: Bool, modifies :: Bool,
        nodeInterface :: NodeInterface,
        edgeInterface :: EdgeInterface,
        structure :: [(NodeName, NodeDegreeClassification)]}
    deriving Show


nodeInterfaceElem :: NodeKey -> RuleCharacterisation -> Bool
nodeInterfaceElem n rc = n `elem` (map fst $ nodeInterface rc)

edgeInterfaceElem :: EdgeKey -> RuleCharacterisation -> Bool
edgeInterfaceElem e rc = e `elem` (map fst $ edgeInterface rc)


edgeClassesFor :: RuleCharacterisation -> NodeName -> NodeDegreeClassification
edgeClassesFor rc id = case lookup id $ structure rc of
    Just ndc -> ndc
    Nothing  -> error $ "Couldn't find node " ++ id
        

characteriseRule :: Rule -> RuleCharacterisation
characteriseRule r@(Rule _ _ (lhs, rhs) nif eif _) = RuleCharacterisation root mod nif eif struc
    where
        root = areRootsUsed r
        mod = True -- TODO: actually calculate this from the interfaces!
        struc = buildGraphStructure lhs


areRootsUsed :: Rule -> Bool
areRootsUsed (Rule _ _ (lhs, rhs) _ _ _ ) = hasRoot lhs || hasRoot rhs
    where
        hasRoot :: RuleGraph -> Bool
        hasRoot g = any (\(RuleNode _ r _) -> r) $ map snd $ allNodes g



-- Classify edges as in out or loop
data EdgeType = InEdge | OutEdge | LoopEdge | UninterestingEdge deriving Eq

buildGraphStructure :: RuleGraph -> [(NodeKey, NodeDegreeClassification)]
buildGraphStructure g =
    [ (id, ndc) | RuleNode id _ _ <- ns , let ndc = classifyEdgesForNode id es]
    where
        ns = map snd $ allNodes g
        es = map fst $ allEdges g


classifyEdgeForNode :: NodeKey -> EdgeKey -> EdgeType
classifyEdgeForNode nk ek =
    case (nodeNumber nk == (nodeNumber $ source ek), nodeNumber nk == (nodeNumber $ target ek)) of
        (True, True) -> LoopEdge
        (True, _)    -> OutEdge
        (_, True)    -> InEdge
        _            -> UninterestingEdge

classifyEdgesForNode :: NodeKey -> [EdgeKey] -> NodeDegreeClassification
classifyEdgesForNode id es = (outs, ins, loops)
    where
        ins   = length $ filter (==InEdge) cs
        outs  = length $ filter (==OutEdge) cs
        loops = length $ filter (==LoopEdge) cs
        cs = map (classifyEdgeForNode id) es
