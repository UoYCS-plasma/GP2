module GPGraph (unlabelled, k) where

import GPSyntax
import Graph

unlabelled :: GPHostLabel
unlabelled  =  GPHostLabel [] Uncoloured

-- the unlabelled graph K_n
k :: Int -> GPHostGraph
k n  =
  foldr (\(n1,n2) g -> fst $ newEdge g n1 n2 unlabelled) isolated edgeEnds
  where
  isolated :: GPHostGraph
  isolated  =  foldr (\i g -> fst $ newNode g unlabelled) emptyGraph [1..n]
  d        :: [NodeId]
  d         =  allNodes isolated
  edgeEnds :: [(NodeId,NodeId)]
  edgeEnds  =  [(n1,n2) | n1 <- d, n2 <- d, n1 /= n2]

  
