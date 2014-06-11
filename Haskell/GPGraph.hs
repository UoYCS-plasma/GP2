module GPGraph (unlabelled, k) where

import GPSyntax
import Graph

unlabelled :: HostLabel
unlabelled  =  HostLabel [] Uncoloured

blankNode :: HostNode
blankNode = HostNode [] False unlabelled

-- the unlabelled graph K_n
k :: Int -> HostGraph
k n  =
  foldr (\(n1,n2) g -> fst $ newEdge g n1 n2 unlabelled) isolated edgeEnds
  where
  isolated :: HostGraph
  isolated  =  foldr (\i g -> fst $ newNode g blankNode) emptyGraph [1..n]
  d        :: [NodeId]
  d         =  allNodes isolated
  edgeEnds :: [(NodeId,NodeId)]
  edgeEnds  =  [(n1,n2) | n1 <- d, n2 <- d, n1 /= n2]

  
