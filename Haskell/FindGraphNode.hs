module FindGraphNode where

import GPSyntax
import Graph


idToNodeId :: RuleGraph -> ID -> NodeID
idToNodeId g id = case candidates of
        [] -> error $ "ID " ++ id ++ " not found"
        [nid] -> nid
        _  -> error $ "Duplicate ID found! Eep!"
    where
        candidates = filter matchID $ allNodes g
        matchID :: RuleNode -> Bool
        matchID (RuleNode i _ _) = i == id



nodeIdToId :: RuleGraph -> NodeId -> ID
nodeIdToId g nid = case nLabel g nid of
    Nothing -> error "FAil!"
    Just ( RuleNode id _ _ ) -> id


