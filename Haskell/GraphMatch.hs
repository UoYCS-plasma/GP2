module GraphMatch where

import Prelude hiding (lookup)
import Data.List
import Data.Maybe
import Control.Monad (guard)


import ExAr
import Graph
import Mapping
import LabelMatch
import GPSyntax

type RuleNodeId = NodeId 
type HostNodeId = NodeId
type RuleEdgeId = EdgeId
type HostEdgeId = EdgeId

type NodeMatches = Mapping RuleNodeId HostNodeId
type EdgeMatches = Mapping RuleEdgeId HostEdgeId


data GraphMorphism = GM Environment NodeMatches EdgeMatches deriving (Show) 

data NodeMorphism = NM Environment NodeMatches deriving (Show)


-- matchGraphEdges h r generates a list of GraphMorphisms from a single
-- NodeMorphism. In order to generate the complete list of GraphMorphisms,
-- we concatMap matchGraphEdges h r over the NodeMorphism list obtained
-- from the call to matchGraphNodes.
-- In the case that the rule graph is the empty graph, we immediately
-- return the empty morphism.

matchGraphs :: HostGraph -> RuleGraph -> [GraphMorphism]
matchGraphs h r = case allNodes r of 
        [] -> [ GM [] [] [] ]
        _ -> concatMap (matchGraphEdges h r) (matchGraphNodes h r)


-- Outputs all valid (w.r.t labels) injective morphisms (node morphisms)
-- from the nodes of LHS to the nodes of the host graph.
--
-- We generate all candidate sets of nodes in the host graph. For an LHS with
-- k nodes, this is the set of size-k subsets of the node set of the host graph,
-- including permutations.
--
-- These subsets are zipped with the node set of the LHS to create the complete
-- set of candidate node morphisms (allNodeMatches). Morphisms that map LHS
-- root nodes to non-root nodes in the host graph are filtered from the set. 
-- The remaining node morphisms are tested with labelMatch.
--
-- labelMatch returns a Maybe Environment. We use the maybe function to lift
-- each Maybe Environment into a Maybe NodeMorphism. We call catMaybes
-- on the resulting list in order to filter out the Nothings.

matchGraphNodes :: HostGraph -> RuleGraph -> [NodeMorphism]
matchGraphNodes h r = -- filter (\m -> case m of NM _ [] -> False ; _ -> True) $
    catMaybes [ nm | nodeMatches <- filter (checkNodes h r) $ allNodeMatches,
                 let maybeEnv = foldr labelMatch (Just []) nodeMatches
                     nm = maybe Nothing (\env -> Just (NM env nodeMatches) ) maybeEnv ]
    where
        rns = allNodes r
        hns = allNodes h
        nodeSets = permutedSizedSubsets (length rns) hns
        allNodeMatches = map (zip rns) nodeSets

        -- Match node labels, further building the environment of variable-value mappings.
        labelMatch :: (RuleNodeId, HostNodeId) -> Maybe Environment -> Maybe Environment
        labelMatch (rn, hn) menv = do
                  env <- menv
                  mapping <- doNodesMatch h r hn rn
                  mergeMapping mapping env

checkNodes :: HostGraph -> RuleGraph -> [(RuleNodeId, HostNodeId)] -> Bool
checkNodes h r nms =  (checkRootNodes h r nms) && (checkJoiningEdges h r nms) && (checkMarks h r nms)

checkRootNodes :: HostGraph -> RuleGraph -> [(RuleNodeId, HostNodeId)] -> Bool
checkRootNodes _ _ [] = True
checkRootNodes h r ((rid, hid):nms) =
     case (rb, hb) of
     -- A node mapping is invalid only in the case where a LHS root node is 
     -- mapped to a non-root host graph node.
     (True, False) -> False
     _ -> True && checkRootNodes h r nms 
     where
         RuleNode _ rb _ = nLabel r rid
         HostNode _ hb _ = nLabel h hid

checkDegrees :: HostGraph -> RuleGraph -> [(RuleNodeId, HostNodeId)] -> Bool 
checkDegrees h r ((rid, hid): nms) = 
    length (inEdges r rid) <= length (inEdges h hid) &&  
    length (outEdges r rid) <= length (outEdges h hid) 



checkJoiningEdges :: HostGraph -> RuleGraph -> [(RuleNodeId, HostNodeId)] -> Bool
checkJoiningEdges h r nms = all wiffle nms
    where
        wiffle :: (RuleNodeId, HostNodeId) -> Bool
        wiffle (rid, hid) = lhes >= lres
            where
                lhes = length $ filter blah $ [ (res, hes) | res <- outEdges r rid, hes <- outEdges h hid ]
                lres = length $ outEdges r rid

        blah :: (RuleEdgeId, HostEdgeId) -> Bool
        blah (reid, heid) = (target r reid, target h heid) `elem` nms

{-
checkJoiningEdges :: HostGraph -> RuleGraph -> [(RuleNodeId, HostNodeId)] -> [(RuleNodeId, HostNodeId)] -> Bool
checkJoiningEdges h r nms ((rid, hid):nms') =
    where
        rEdges = outEdges r rid
        rTargets = map (target r) rEdges
        hEdges = outEdges h hid
        hTargets = map (target h) hEdges
   
        [ (rn, hn) | rn <- rTargets, hn <- hTargets ]
        -- We need the above list and nms' to "line up", whatever that means.
        

        rn = length rEdges
        hEdges = filter (someFunction) $ outEdges h hid
        -- foreach rule edge there must be a corresponding host edge such that
        --   the target of the rule edge must match the target of the host edge
        someFunction :: HostEdgeId -> Bool
        someFunction he = 
                rn = ... $ source h he
                target h he
        rids, hids = unzip nms

f :: HostGraph -> RuleGraph -> (RuleNodeId, HostNodeId) -> (RuleNodeId, HostNodeId) -> Bool
f h r (rn1, hn1) (rn2, hn2) = null (joiningEdges r rn1 rn2) => null (joiningEdges h hn1 hn2)
-}        

checkMarks :: HostGraph -> RuleGraph -> [(RuleNodeId, HostNodeId)] -> Bool 
checkMarks h r ((rid, hid): nms) = colourMatch hc rc
    where
        RuleNode _ _ (RuleLabel _ rc) = nLabel r rid
        HostNode _ _ (HostLabel _ hc) = nLabel h hid

                
doNodesMatch :: HostGraph -> RuleGraph -> HostNodeId -> RuleNodeId -> Maybe Environment
doNodesMatch h r hid rid = 
    let hnode = (maybeNLabel h hid)
        rnode = (maybeNLabel r rid) in
    case (hnode, rnode) of 
         (Nothing, _) -> Nothing
         (_, Nothing) -> Nothing
         (Just (HostNode _ _ hlabel), Just (RuleNode _ _ rlabel)) 
                      -> doLabelsMatch hlabel rlabel


-- For each edge in the RuleGraph, we generate a pair of its source and target
-- node (ruleEndPoints).
-- Each of these pairs is transformed to the source and target node in the host
-- graph using the node morphism (hostEndPoints).
-- joiningEdges is called on each hostEndPoint to generate all the candidate
-- edges for each RuleEdge. A RuleEdge may have more than one candidate
-- HostEdge, so a list of lists is created (hostEdges).
-- hostEdges is used to generate edgeMatches, analogous to nodeMatches in 
-- matchGraphNodes, which is passed to the list comprehension in the same way 
-- as in the node matcher.

matchGraphEdges :: HostGraph -> RuleGraph -> NodeMorphism -> [GraphMorphism]
matchGraphEdges h r (NM env nodeMatches) = if null (allEdges r) then [GM env nodeMatches []] else
   catMaybes [ gm | edgeMatch <- edgeMatches, --not $ null edgeMatch,
                let maybeEnv = foldr labelMatch (Just env) edgeMatch
                    gm = maybe Nothing (\env -> Just (GM env nodeMatches edgeMatch) ) maybeEnv ]
   where 
     ruleEdges = allEdges r
     ruleEndPoints = map (\e -> (source r e, target r e)) ruleEdges
     hostEndPoints = mapMaybe ruleEndsToHostEnds ruleEndPoints
     hostEdges = map (\(src, tgt) -> [eid | eid <- joiningEdges h src tgt]) hostEndPoints

     -- The source and target node aren't guaranteed to exist in the node morphism,
     -- hence we have to use a standard lookup and pattern match on the pair of 
     -- Maybe NodeIds.
     ruleEndsToHostEnds :: (RuleNodeId, RuleNodeId) -> Maybe (HostNodeId, HostNodeId)
     ruleEndsToHostEnds (src, tgt) = case (lookup src nodeMatches, lookup tgt nodeMatches) of
        (Just s, Just t) -> Just (s, t)
        _                -> Nothing

     labelMatch :: (RuleEdgeId, HostEdgeId) -> Maybe Environment -> Maybe Environment
     labelMatch (re, he) menv = do
               env <- menv
               mapping <- doEdgesMatch h r he re
               mergeMapping mapping env
            
     -- The kth edge ID in ruleEdges corresponds to the kth list of host edge
     -- IDs in hostEdges. They are combined into a list containing all possible
     -- mappings from left-edges to host-edges where each mapping is a list of
     -- pairs. This is achieved by the generateMatches function, defined in 
     -- Graph.hs.
     --
     -- An example input is [LE1, LE2] [[HE1, HE2], [HE3, HE4, HE5]] 
     --
     -- This means that left-edge LE1 has two candidate host-edges to which it could 
     -- be mapped (HE1 and HE2), while LE2 has three candidates (HE3, HE4 and HE5).
     -- 
     -- The output is
     -- [[(LE1, HE1), (LE2, HE3)], [(LE1, HE1), (LE2, HE4)], [[(LE1, HE1), (LE2, HE5)],
     --  [(LE1, HE2), (LE2, HE3)], [(LE1, HE2), (LE2, HE4)], [(LE1, HE2), (LE2, HE5)]]
     -- 
     -- Each inner list describes a mapping from the complete set of left-edges to
     -- an appropriate set of host-edges.
     edgeMatches = generateMatches ruleEdges hostEdges

doEdgesMatch :: HostGraph -> RuleGraph -> HostEdgeId -> RuleEdgeId -> Maybe Environment
doEdgesMatch h r hid rid = 
   let mhlabel = (maybeELabel h hid)
       mrlabel = (maybeELabel r rid) in
   case (mhlabel, mrlabel) of 
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just hlabel, Just rlabel) -> doLabelsMatch hlabel rlabel
        

