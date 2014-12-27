module OilrMachine.HostCompile where

import GPSyntax
import OilrMachine.Instructions
import OilrMachine.Compile


compileHostGraph :: AstHostGraph -> Prog
compileHostGraph (AstHostGraph ns es) =
    PROC "_HOST" : concatMap (compileHostNode rmap) ns ++ map (compileHostEdge rmap) es ++ [RET]
    where
        rmap = makeRegisterMap 0 ns

makeRegisterMap :: Int -> [HostNode] -> RegisterMap
makeRegisterMap i ((HostNode id _ _):ns) = (id, i) : (makeRegisterMap (i+1) ns)
makeRegisterMap _ [] = []


compileHostNode :: RegisterMap -> HostNode -> Prog
compileHostNode rmap (HostNode id root _) = 
    case (root, lookup id rmap) of
        (True,  Just i) -> [ NEWN, ROOT i ]
        (False, Just i) -> [ NEWN ]
        _               -> error "Referenced node is not in register map!"

compileHostEdge :: RegisterMap -> HostEdge -> Instr
compileHostEdge rmap (HostEdge src tgt _) =
    case (lookup src rmap, lookup tgt rmap ) of
        (Just s, Just t) -> NEWE s t
        _                -> error $ "Endpoint of edge not found in host graph!"
 

