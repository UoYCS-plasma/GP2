module GPGraph (unlabelled, k) where

import GPSyntax
import Graph

type RuleGraph = Graph RuleNode RuleEdge

data RuleNode = RuleNode NodeName Bool RuleLabel deriving (Show, Eq)
data RuleEdge = RuleEdge EdgeName Bool RuleLabel deriving Show

data HostNode = HostNode NodeName Bool HostLabel deriving Show
-- For graph isomorphism checking.
instance Eq HostNode where
    HostNode _ isRoot1 label1 == HostNode _ isRoot2 label2 =
        isRoot1 == isRoot2 && label1 == label2

data HostEdge = HostEdge NodeName NodeName HostLabel deriving Show

-- Host Graph ADTs
type HostGraph = Graph HostNode HostLabel

colourH :: HostGraph -> NodeId -> Colour
colourR r n = c where HostNode _ _ (HostLabel _ c) = nLabel r n 

colourR :: RuleGraph -> NodeId -> Colour
colourR r n = c where RuleNode _ _ (RuleLabel _ c) = nLabel r n 

isRootH :: HostGraph -> NodeId -> Bool 
isRoot r n = r where HostNode _ r _ = nLabel r n 

isRootR :: RuleGraph -> NodeId -> Bool
isRoot r n = r where RuleNode _ r _ = nLabel r n 

bidirectional :: RuleGraph -> EdgeId -> Bool
bidirectional r e = bi where RuleEdge _ bi _ = eLabel r e 

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

  
