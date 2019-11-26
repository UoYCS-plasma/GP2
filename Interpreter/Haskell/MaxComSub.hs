module MaxComSub (maxCommonSubgraph) where
-- Find maximal common subgraphs in the left and right
-- sides of rules.  When the rule is applied the nodes
-- edges in these graphs need not be deleted on the left
-- and need not be created on the right.

import GPSyntax
import Graph
import Mapping
import Data.List

-- Common subgraphs are represented by a node mapping
-- and an edge mapping between them.
type NidMap = Mapping NodeId NodeId
type EidMap = Mapping EdgeId EdgeId

-- An interface mapping provides the initial core of
-- a node mapping.  The problem is to find the best
-- extension of this mapping to include other nodes.
-- The best extension maximises the number of nodes
-- and edges in correspondence.  As the number of
-- non-interface nodes in rule-graphs is typically small,
-- the best extension is found by brute-force search.
maxCommonSubgraph :: Rule -> (NidMap,EidMap)
maxCommonSubgraph (Rule _ _ (lhs,rhs) iface _ _) =
  let lhxNids = allNodes lhs \\ dom iface
      rhxNids = allNodes rhs \\ rng iface
      m = min (length $ lhxNids) (length $ rhxNids)
  in  maximumBy compareNoOfEdges
        [ (nMap, eMap)
        | xnMap <- correspondencesOf m lhxNids rhxNids,
          let nMap = iface ++ xnMap,
          let eMap = commonEdges lhs rhs nMap ]

compareNoOfEdges ::
 (NidMap, EidMap) -> (NidMap, EidMap) -> Ordering
compareNoOfEdges (nm1,em1) (nm2,em2) =
  compare (length em1) (length em2)

correspondencesOf :: Int -> [a] -> [b] -> [Mapping a b]
correspondencesOf k xs ys =
  [ zip xs' ys''
  | xs'  <- sublistsOf k xs,
    ys'  <- sublistsOf k ys,
    ys'' <- permutations ys' ]

-- For a given correspondence between subsets of nodes
-- in left and right rule-graphs, assuming rules without
-- labels, commonEdges finds the largest correspondence
-- between edges.
commonEdges :: RuleGraph -> RuleGraph -> NidMap -> EidMap
commonEdges lhs rhs nMap =
  foldl trytoAdd [] $ allEdges lhs
  where
  trytoAdd eMap lhe =
    let lhsrc  = source lhs lhe
        lhtgt  = target lhs lhe
        rhsrc  = definiteLookup lhsrc nMap
        rhtgt  = definiteLookup lhtgt nMap
        rhes   = joiningEdges rhs rhsrc rhtgt \\ rng eMap
        rhe    = head rhes
    in  if lhsrc `elem` dom nMap && lhtgt `elem` dom nMap &&
           not (null rhes)
        then addItem eMap lhe rhe
        else eMap              

-- These utility functions dom and rng should really be
-- defined in the Mapping module.

dom :: Mapping a b -> [a]
dom = map fst

rng :: Mapping a b -> [b]
rng = map snd
