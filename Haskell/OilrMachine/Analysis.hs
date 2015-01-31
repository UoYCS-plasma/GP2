module OilrMachine.Analysis (RuleCharacterisation, characteriseRule, nodeInterfaceElem, edgeInterfaceElem, modifies) where

import GPSyntax
import Data.List

type Interface = [String]

data Characteristic = Unused | MinMax Int Int

data RuleCharacterisation = RuleCharacterisation {
        rooted :: Bool, modifies :: Bool,
        nodeInterface :: Interface,
        edgeInterface :: Interface }


nodeInterfaceElem :: NodeName -> RuleCharacterisation -> Bool
nodeInterfaceElem n rc = n `elem` nodeInterface rc

edgeInterfaceElem :: EdgeName -> RuleCharacterisation -> Bool
edgeInterfaceElem e rc = e `elem` edgeInterface rc


characteriseRule :: AstRule -> RuleCharacterisation
characteriseRule r@(AstRule _ _ (lhs, rhs) _) = RuleCharacterisation root mod nif eif
    where
        root = areRootsUsed r
        (nif, eif, mod) = computeInterfaces lhs rhs


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


