module GraphMatch where

import Prelude hiding (lookup)
import Data.List
import Data.Maybe
import Control.Monad (guard)
import LabelMatch

import ExAr
import GPGraph
import Graph
import GPSyntax

type RuleNodeId = NodeId
type HostNodeId = NodeId
type RuleEdgeId = EdgeId
type HostEdgeId = EdgeId

-- type Environment = Subst ID [HostAtom]

type NodeMatches = Subst RuleNodeId HostNodeId
type EdgeMatches = Subst RuleEdgeId HostEdgeId


data GraphMorphism = GM Environment NodeMatches EdgeMatches

data NodeMorphism = NM Environment NodeMatches
--                  NM env nodeMatches


doNodesMatch :: HostGraph -> RuleGraph -> HostNodeId -> RuleNodeId -> Maybe Environment
doNodesMatch h r hid rid = doLabelsMatch hlab rlab
    where  -- todo: add error checking!
        HostNode _ _ hlab = fromJust (nLabel h hid)
        RuleNode _ _ rlab = fromJust (nLabel r rid)

-- Also check for matching source and target? TODO: add error checking
doEdgesMatch :: HostGraph -> RuleGraph -> HostEdgeId -> RuleEdgeId -> Maybe Environment
doEdgesMatch h r hid rid = doLabelsMatch (fromJust $ eLabel h hid) (fromJust $ eLabel r rid)

matchGraphNodes :: HostGraph -> RuleGraph -> [NodeMorphism]
matchGraphNodes h r = 
    [NM env nodeMatches
        | ss <- sss ,  -- s for subset!
        let nodeMatches = zip rns ss
            Just env = foldr labMatch (Just []) nodeMatches ]
    where
        rns = allNodes r
        hns = allNodes h
        sss = permutedSizedSubsets (length rns) hns

        labMatch :: (RuleNodeId, HostNodeId) -> Maybe Environment -> Maybe Environment
        labMatch (rn, hn) ms = do
                s <- ms -- s for subst
                s' <- doNodesMatch h r hn rn
                substMerge s s' 
                

matchGraphEdges :: HostGraph -> RuleGraph -> NodeMorphism -> [GraphMorphism]
matchGraphEdges h r (NM env nodeMatches) = concatMap getGMsForNode nodeMatches
    where
        getGMsForNode :: ( RuleNodeId, HostNodeId ) -> [GraphMorphism]
        getGMsForNode ( rn, hn ) = [GM env' nodeMatches edgeMatches
            | ss <- sss ,
            let edgeMatches = zip res ss
                Just env' = foldr labMatch (Just env) edgeMatches ]
            where
                res = outEdges r rn
                -- Find corresponding host graph node, and all size-n permuted subsets of outgoing edges. 
                hes = outEdges h hn
                sss = permutedSizedSubsets ( length res) hes

        --      Match edge labels, further building the environment of Substs
        labMatch :: (RuleEdgeId, HostEdgeId) -> Maybe (Environment) -> Maybe (Environment)
        labMatch (re, he) env = do
            e <- env
            e' <- doEdgesMatch h r he re
            substMerge e e'

permutedSizedSubsets :: Int -> [a] -> [[a]]
permutedSizedSubsets k xs = concatMap perms $ sublistsOf k xs

sublistsOf :: Int -> [a] -> [[a]]
sublistsOf 0 _        = [[]]
sublistsOf _ []       = []
sublistsOf n (x:xs)   = map (x:) (sublistsOf (n-1) xs) ++ sublistsOf n xs

perms :: [a] -> [[a]]
perms []      =  [[]]
perms xs      =  [x:p | (x,xs') <- picks xs, p <- perms xs']

picks :: [a] -> [(a,[a])]
picks []      =  []
picks (x:xs)  =  (x,xs) : [(x',x:xs') | (x',xs') <- picks xs]

matchGraphs :: HostGraph -> RuleGraph -> [GraphMorphism]
matchGraphs h r = concatMap (matchGraphEdges h r) $ matchGraphNodes h r


{-
-- Returns every hostNode that matches a given ruleNode.
matchRuleNode :: HostGraph -> RuleGraph -> RuleNodeId -> [NodeMatch]
matchRuleNode h r rn =
    [ (rn, n, fromJust $ doNodesMatch h r n rn) | n <- allNodes h , isJust $ doNodesMatch h r n rn ]

-- Returns every hostEdge that matches a given ruleEdge
matchRuleEdge :: HostGraph -> RuleGraph -> RuleEdgeId -> [EdgeMatch]
matchRuleEdge h r re =
    [ (re, he, fromJust $ doEdgesMatch h r he re) | he <- allEdges h , isJust $ doEdgesMatch h r he re ]

-- Needs to be permuted with matchRuleEdge, and then impossible matches (where
--   nodes and edges are incompatible) need to be filtered out.
matchNodes :: HostGraph -> RuleGraph -> [[NodeMatch]]
matchNodes h r = map ( matchRuleNode h r ) $ allNodes r

matchEdges :: HostGraph -> RuleGraph -> [[EdgeMatch]]
matchEdges h r = map ( matchRuleEdge h r ) $ allEdges r

-- should be taking a search-plan approach: start with an arbitrary node
--   and extend the match as far as possible
matchGraph :: HostGraph -> RuleGraph -> [GraphMorphism]
matchGraph = notImplemented
--matchGraph g r = [ (nm, em) | nm <- matchNodes g r , em <- matchEdges g r ]
-}


-- matching the graph is a generalisation of a parsing task! 
-- Our parser consumes a host graph _and_ a rule graph, returning the unconsumed parts of the graphs
-- plus a possible NodeMatch or EdgeMatch

{-
type Matcher a = RuleGraph -> HostGraph -> [(RuleGraph, HostGraph, a)]

pure :: a -> Matcher a
pure x = \r h -> [(r, h, x)]

infixl 4 °

(°) :: Matcher (a -> b) -> Matcher a -> Matcher b
f ° a = \h -> [(h1, g b) | (h0, g) <- f h, (h1, b) <- a h0]

matchRuleNode :: RuleGraph -> RuleNodeId -> Matcher NodeMatch
matchRuleNode r rnid h = notImplemented
    where
        r' = rmNode r rnid

matchRuleEdge :: RuleGraph -> RuleEdgeId -> Matcher EdgeMatch
matchRuleEdge r reid h = notImplemented
    where
        r' = rmEdge r reid

matchGraph :: RuleGraph -> Matcher GraphMorphism
matchGraph r h = notImplemented
-}

{-



-- two nodes are equal if their labels are equal
nodesMatch g r gn rn = nLabel g gn == nLabel r rn

-- two edges are equal if their labels are equal AND the nodes on either end are equal
edgesMatch :: HostGraph -> RuleGraph -> HostEdgeId -> RuleEdgeId -> Bool
edgesMatch g r ge re = ( eLabel g ge == eLabel r re )
                       && ( nodesMatch g r (fromJust $ source g ge) (fromJust $ source r re) )
                       && ( nodesMatch g r (fromJust $ target g ge) (fromJust $ target r re) )
       
matchRuleNode :: HostGraph -> RuleGraph -> RuleNodeId -> NodeMatches
matchRuleNode g r rn =
    [ (rn, n) | n <- allNodes g , nodesMatch g r n rn ]

matchRuleEdge :: HostGraph -> RuleGraph -> RuleEdgeId -> EdgeMatches
matchRuleEdge g r re =
    [ (re, e) | e <- allEdges g , edgesMatch g r e re ]

matchNodes :: HostGraph -> RuleGraph -> NodeMatches
matchNodes g r = concatMap ( matchRuleNode g r ) $ allNodes r

matchEdges :: HostGraph -> RuleGraph -> EdgeMatches
matchEdges g r = concatMap ( matchRuleEdge g r ) $ allEdges r
{-matchEdges g r (rn, gn) = union ins outs
    where
        ins  = filter (uncurry $ edgesMatch g r)
                    [ (re, e) | e <- inEdges g gn, re <- inEdges r rn ]
        outs = filter (uncurry $ edgesMatch g r)
                    [ (re, e) | e <- outEdges g gn, re <- outEdges r rn ] -}

matchGraph :: HostGraph -> RuleGraph -> [GraphMorphism]
matchGraph g r = [ (nm, em) | nm <- matchNodes g r , em <- matchEdges g r ]

-}                


