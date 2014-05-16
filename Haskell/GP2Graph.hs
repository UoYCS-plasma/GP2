module GP2Graph (GP2Graph, GP2HostLabel(..), IntOrStr(..), Colour(..), unlabelled, k) where

import Graph

data GP2HostLabel = GP2HostLabel [IntOrStr] Colour deriving (Eq, Show)

data GP2Atom = Int Int | Str String | Chr Char deriving (Eq, Show)

{- Colours have yet to be formalised. Currently working on the assumption that only one can be applied to a given edge or node -}
data Colour   = Uncoloured | Red | Green | Blue | Grey | Cyan | Dashed deriving (Eq, Show)

type GP2Graph = Graph GP2HostLabel

unlabelled :: GP2HostLabel
unlabelled  =  GP2HostLabel [] Uncoloured

-- the unlabelled graph K_n
k :: Int -> GP2Graph
k n  =
  foldr (\(n1,n2) g -> fst $ newEdge g n1 n2 unlabelled) isolated edgeEnds
  where
  isolated :: GP2Graph
  isolated  =  foldr (\i g -> fst $ newNode g unlabelled) emptyGraph [1..n]
  d        :: [NodeId]
  d         =  allNodes isolated
  edgeEnds :: [(NodeId,NodeId)]
  edgeEnds  =  [(n1,n2) | n1 <- d, n2 <- d, n1 /= n2]

  
