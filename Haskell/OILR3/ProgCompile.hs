module OILR3.ProgCompile (compileProgram) where

import OILR3.Instructions

import GPSyntax
import Mapping

import Data.List
import Data.Maybe
import Debug.Trace

data GraphElem = N NodeKey | E EdgeKey deriving (Show, Eq)
type GraphElemId = (AstRuleGraph, GraphElem)
type SemiOilrCode = [Instr GraphElemId GraphElemId]
type OilrCode = [Instr Int Int]

type NodeKey = String
type EdgeKey = (String, String, String)

type Interface = [String]

notImplemented n = error $ "Not implemented: " ++ show n


compileProgram :: [Flag] -> GPProgram -> [OilrCode]
compileProgram flags (Program ds) = map (insertOrbs [] Nothing . postprocess) mappings
    where
        prog = map oilrCompileDeclaration ds -- $ trace (show lifted) lifted
        lifted = oilrHeavyLifter ds
        mappings = map elemIdMapping prog


mergeTravs :: SemiOilrCode -> SemiOilrCode -> SemiOilrCode
mergeTravs nts []  = nts
mergeTravs nts ets = edgesToInstrs [] [] edges
    where
        edges  = [ (src, ed, tgt)
                    | src@(LUN sn _)   <- nts
                    , tgt@(LUN tn _)   <- nts
                    , ed@(LUE e s t _) <- ets
                    , sn==s , tn==t ]

-- TODO: when we use LUN to find a node with loops, and then don't modify the
-- loops, OILR indexing guarantees that those loops exist (which is all we care
-- about). Therefore with full indexing enabled, we can save some bind ops by
-- not bothering to match the loops.

edgesToInstrs acc _ [] = reverse acc
edgesToInstrs acc seen ((LUN _ sp, LUE e s t d, LUN _ tp):es) =
    case (s==t, s `elemIndex` seen, t `elemIndex` seen) of
        (_,     Just _ , Just _ ) -> edgesToInstrs (LUE e s t d:acc)          seen     es
        (True,  Nothing, _      ) -> edgesToInstrs (LUE e s t d:LUN s sp:acc) (s:seen) es
        (False, Nothing, Just _ ) -> edgesToInstrs (XTE t e s In:acc)         (s:seen) es
        (False, Just _ , Nothing) -> edgesToInstrs (XTE s e t Out:acc)        (t:seen) es
        (False, Nothing, Nothing) -> edgesToInstrs (XTE s e t Out:LUN s sp:acc) (t:s:seen) es
edgesToInstrs _ _ ((_, LUE _ _ _ In, _):es) = error "Found an unexpected in-edge"

insertOrbs :: OilrCode -> Maybe Int -> OilrCode -> OilrCode
insertOrbs acc prev [] = reverse acc
insertOrbs acc prev (i:is) = case (i, prev) of
    ( LUN n pr    , Nothing ) -> insertOrbs (ORF:i:acc) (Just n) is
    ( LUN n pr    , Just p  ) -> insertOrbs (ORB p:i:acc) (Just n) is
    ( XTE _ e _ _ , Just p  ) -> insertOrbs (ORB p:i:acc) (Just e) is
--    ( XOE _ e _   , Just p  ) -> insertOrbs (ORB p:i:acc) (Just e) is
--    ( XIE _ e _   , Just p  ) -> insertOrbs (ORB p:i:acc) (Just e) is
    ( LUE n _ _ _ , Just p  ) -> insertOrbs (ORB p:i:acc) (Just p) is -- don't update the jump point!
    ( NEC n _     , Just p  ) -> insertOrbs (ORB p:i:acc) (Just p) is
    ( LUE _ _ _ _ , Nothing ) -> error "Tried to match an edge before a node"
    ( XTE _ _ _ _ , Nothing ) -> error "Nothing to extend!"
    -- ( XIE _ _ _   , Nothing ) -> error "Extend-in instruction cannot be first"
    -- ( XOE _ _ _   , Nothing ) -> error "Extend-out instruction cannot be first"
    _                       -> insertOrbs (i:acc) prev is
        

-- TODO: is there a reason we can't apply this transformation to node and edge
-- identifiers before we compile? I have a recollection that there might be, but
-- I didn't write down what it was... drat.
postprocess :: (Mapping GraphElemId Int, SemiOilrCode) -> OilrCode
postprocess (mapping, sois) = map postprocessInstr sois
    where
        translate :: GraphElemId -> Int
        translate id = definiteLookup id mapping

        postprocessInstr :: Instr GraphElemId GraphElemId -> Instr Int Int
        postprocessInstr (PRO s)     = PRO s
        postprocessInstr (RUL s)     = RUL s
        postprocessInstr (ALP s)     = ALP s
        postprocessInstr (CAL s)     = CAL s
        postprocessInstr UBA         = UBA
        postprocessInstr END         = END
        postprocessInstr (ADN n)     = ADN $ translate n
        postprocessInstr (ADE e s t) = ADE (translate e) (translate s) (translate t)
        postprocessInstr (DEN n)     = DEN $ translate n
        postprocessInstr (DEE e)     = DEE $ translate e
        postprocessInstr (RTN n)     = RTN $ translate n
        postprocessInstr (URN n)     = URN $ translate n
        postprocessInstr (CON n c)   = CON (translate n) c
        postprocessInstr (LUN n p)   = LUN (translate n) p
        postprocessInstr (LUE e s t d) = LUE (translate e) (translate s) (translate t) d
        postprocessInstr (XTE s e t d) = XTE (translate s) (translate e) (translate t) d
        -- postprocessInstr (XOE s e t) = XOE (translate s) (translate e) (translate t)
        -- postprocessInstr (XIE t e s) = XIE (translate t) (translate e) (translate s)
        postprocessInstr (NEC n1 n2) = NEC (translate n1) (translate n2)
        postprocessInstr OK          = OK
        postprocessInstr TRU         = TRU
        postprocessInstr FLS         = FLS
        postprocessInstr RET         = RET
        postprocessInstr i           = error $ show i ++ " is not implmented"

elemIdMapping :: SemiOilrCode -> (Mapping GraphElemId Int, SemiOilrCode)
elemIdMapping sois = ( zip (nub [ id | id <- concatMap extractId sois ]) [0,1..], sois )

extractId :: Instr a a -> [a]
extractId (ADN n)       = [n]
extractId (DEN n)       = [n]
extractId (RTN n)       = [n]
extractId (LUN n _)     = [n]
extractId (ADE e _ _)   = [e]
extractId (DEE e)       = [e]
extractId (LUE e _ _ _) = [e]
-- extractId (XOE _ e t)  = [e, t]
-- extractId (XIE _ e s)  = [e, s]
extractId _            = []



-- -------------------------------------------------------------------
-- program OILR instruction generation
-- -------------------------------------------------------------------

oilrCompileDeclaration :: Declaration -> SemiOilrCode
oilrCompileDeclaration m@(Main _)      = oilrCompileMain m
oilrCompileDeclaration p@(Proc _ _ _)  = oilrCompileProc p
oilrCompileDeclaration (AstRuleDecl r) = oilrCompileRule r

oilrCompileProc :: Declaration -> SemiOilrCode
oilrCompileProc (Proc name ds cs) = (PRO name : concatMap oilrCompileExpr cs) ++ [END]

oilrCompileMain :: Declaration -> SemiOilrCode
oilrCompileMain (Main cs) = oilrCompileProc (Proc "Main" [] cs)

-- For (IfStatement cn th el), where cn is a single rule, we can introduce a new rule cn' which performs the matching part of the rule but not the transformation, as the output of cn will be discarded anyway.

oilrCompileExpr :: Expr -> SemiOilrCode
oilrCompileExpr (IfStatement  cn th el)    = notImplemented 2
oilrCompileExpr (TryStatement cn th el)    = notImplemented 3
oilrCompileExpr (Sequence es)              = concatMap oilrCompileExpr es
oilrCompileExpr (ProgramOr a b)            = notImplemented 6
oilrCompileExpr (RuleSet [r])              = [ CAL r ]
oilrCompileExpr (ProcedureCall p)          = [ CAL p ]
oilrCompileExpr Skip                       = [ TRU, RET ]
oilrCompileExpr Fail                       = [ FLS, RET ]
oilrCompileExpr (Looped (RuleSet rs))      = [ ALP r | r <- rs ] -- TODO: not nondeterministic!
oilrCompileExpr (Looped (ProcedureCall p)) = [ ALP p ]
oilrCompileExpr (Looped e)                 = error $ "Invalid Loop construct " ++ show e
oilrCompileExpr e                          = error $ "Unsupported: " ++ show e


-- oilrHeavyLifter promotes sequences to procedures
oilrHeavyLifter :: [Declaration] -> [Declaration]
oilrHeavyLifter [] = []
oilrHeavyLifter (d:ds) = case d of
    Main es         -> Main es' : oilrHeavyLifter (newDs++ds)
        where (es', newDs) = oilrLifter es []
    Proc lbl scp es -> Proc lbl scp es' : oilrHeavyLifter (newDs++ds)
        where (es', newDs) = oilrLifter es []
    d               -> d : oilrHeavyLifter ds

oilrLifter :: [Expr] -> [(Expr, [Declaration])] -> ([Expr], [Declaration])
oilrLifter (Sequence s:es) acc = oilrLifter es ((ProcedureCall pr, [Proc pr [] s]):acc)
    where pr = "Proc__" ++ show (length acc)
oilrLifter (e:es)          acc = oilrLifter es ((e, []):acc) 
oilrLifter []              acc = (es, concat dss)
    where (es, dss) = unzip acc



-- -------------------------------------------------------------------
-- rule compilation is complex...
-- -------------------------------------------------------------------

nodeIds :: AstRuleGraph -> Interface
nodeIds (AstRuleGraph ns _)   = [ id | (RuleNode id _ _) <- ns ]

edgeIds :: AstRuleGraph -> [EdgeKey]
edgeIds (AstRuleGraph _ es)   = [ (id, src, tgt) | (AstRuleEdge id _ src tgt _) <- es ]

rootNodes :: AstRuleGraph -> [NodeKey]
rootNodes (AstRuleGraph ns _) = [ nk | (RuleNode nk root _) <- ns , root == True ]

bidiEdges :: AstRuleGraph -> [EdgeKey]
bidiEdges (AstRuleGraph _ es) = [ (id, src, tgt) | (AstRuleEdge id bidi src tgt _) <- es , bidi == True ]

nodeColours :: AstRuleGraph -> Mapping NodeKey Colour
nodeColours (AstRuleGraph ns _) = [ (nk, c) | (RuleNode nk _ (RuleLabel _ c)) <- ns ]

source :: EdgeKey -> NodeKey
source (_, nk, _) = nk

target :: EdgeKey -> NodeKey
target (_, _, nk) = nk

inDegree :: AstRuleGraph -> NodeKey -> Int
inDegree  g nk = length [ ek | ek <- edgeIds g , target ek == nk , not $ isBidi g ek ]

outDegree :: AstRuleGraph -> NodeKey -> Int
outDegree g nk = length [ ek | ek <- edgeIds g , source ek == nk , not $ isBidi g ek ]

loopCount :: AstRuleGraph -> NodeKey -> Int
loopCount g nk = length [ ek | ek <- edgeIds g , source ek == nk && target ek == nk ]

isRoot :: AstRuleGraph -> NodeKey -> Bool
isRoot g nk = nk `elem` rootNodes g

colour :: AstRuleGraph -> NodeKey -> Colour
colour g nk = definiteLookup nk $ nodeColours g

isBidi :: AstRuleGraph -> EdgeKey -> Bool
isBidi g ek@(_,s,t) = ek `elem` bidiEdges g  &&  s /= t -- a bidi loop is just a loop!


oilrCompileRule :: AstRule -> SemiOilrCode
oilrCompileRule r@(AstRule name _ (lhs, rhs) cond) = ( [RUL name] ++ body ++ [UBA, END] )
    where
        nif  = nodeIds lhs `intersect` nodeIds rhs
        body = left ++ oilrCompileCondition lhs cond ++ (OK:oilrCompileRhs lhs rhs nif)
        left = oilrCompileLhs cond lhs nif

oilrCompilePredicateRule :: AstRule -> SemiOilrCode
oilrCompilePredicateRule r@(AstRule name _ (lhs, rhs) cond) = ( [RUL name] ++ body ++ [UBA, END] )
    where
        nif  = nodeIds lhs `intersect` nodeIds rhs
        body = oilrCompileLhs cond lhs nif ++ oilrCompileCondition lhs cond

-- Make sure the most constrained nodes are looked for first
oilrSortNodeLookups :: SemiOilrCode -> SemiOilrCode
oilrSortNodeLookups is = reverse $ sortBy mostConstrained is
    where
        mostConstrained (LUN _ p1) (LUN _ p2) = comparePreds p1 p2

oilrSortEdgeLookups :: SemiOilrCode -> SemiOilrCode
oilrSortEdgeLookups is = interesting ++ uninteresting
    where
        (interesting, uninteresting) = partition isInteresting is
        isInteresting (LUE _ s t d) | s == t    = True  -- loop
                                    | otherwise = length [ e | LUE e s' t' d <- is --parallel
                                                           ,  s==s' && t==t'
                                                           || s==t' && t==s' ] > 1

-- NOTE: haskell's standard compare on n-tuples doesn't give good results
-- so I've developed a custom points-based approach
comparePreds :: Pred -> Pred -> Ordering
comparePreds p1 p2 = compare (predToWeight p1) (predToWeight p2)

predToWeight :: Pred -> Int
predToWeight pr = 4 * valueForDim r + 2*valueForDim l + (sum $ map valueForDim [o,i])
    where o = oDim pr 
          i = iDim pr
          l = lDim pr
          r = rDim pr
          valueForDim :: Dim -> Int
          valueForDim (GtE n) = n
          valueForDim (Equ n) = (n+1)*2

applyConds :: Condition -> NodeName -> Pred -> Pred
applyConds (Eq [Indeg n] [Val (Int v)]) name pr = pr { iDim = Equ v }
applyConds _ _ p = p

oilrCompileLhs :: Condition -> AstRuleGraph -> Interface -> SemiOilrCode
oilrCompileLhs cs lhs nif = code
    where
        code   = mergeTravs nTravs eTravs
        eTravs = oilrSortEdgeLookups $ map compileEdge (edgeIds lhs)
        nTravs = oilrSortNodeLookups $ map compileNode (nodeIds lhs)
        compileNode :: NodeName -> Instr GraphElemId GraphElemId
        compileNode nk = cn
             where
                cn = if nk `elem` nif
                        then LUN (oilrNodeId lhs nk) $ applyConds cs nk (Pred { oDim=GtE o, iDim=GtE i, lDim=GtE l, rDim=r , cDim=c } )
                        else LUN (oilrNodeId lhs nk) $ applyConds cs nk (Pred { oDim=Equ o, iDim=Equ i, lDim=Equ l, rDim=r , cDim=c } )
                o = outDegree lhs nk - l
                i = inDegree lhs nk - l
                l = loopCount lhs nk
                r = if isRoot lhs nk then Equ 1 else GtE 0
                c = definiteLookup (colour lhs nk) colourMapping
        compileEdge ek
            | isBidi lhs ek = LUE (oilrEdgeId lhs ek) (oilrNodeId lhs $ source ek) (oilrNodeId lhs $ target ek) Either
            | otherwise     = LUE (oilrEdgeId lhs ek) (oilrNodeId lhs $ source ek) (oilrNodeId lhs $ target ek) Out

oilrCompileCondition :: AstRuleGraph -> Condition -> SemiOilrCode
oilrCompileCondition _ NoCondition = []
oilrCompileCondition lhs (Not (Edge a b Nothing)) = [NEC (oilrNodeId lhs a) (oilrNodeId lhs b)]
oilrCompileCondition _ (Eq _ _) = []  -- rule atoms handled in applyConds
oilrCompileCondition _ c = trace ("\n ** Skipping condition " ++ show c) []

-- Note that when making unique IDs from the RHS we _always_ use the LHS graph,
-- not the RHS.  The inclusion of the graph in the unique ID is purely to
-- identify elements that belong to same rule, and should not be used for any
-- other purpose!
oilrCompileRhs :: AstRuleGraph -> AstRuleGraph -> Interface -> SemiOilrCode
oilrCompileRhs lhs rhs nif = edgeDeletions ++ nodeDeletions ++ nodeInsertions ++ edgeInsertions ++ newRoots ++ unRoots ++ colourChanges
    where
        nodeDeletions  = [ DEN (oilrNodeId lhs nk) | nk <- nodeIds lhs 
                                                   , not (nk `elem` nif) ]
        nodeInsertions = [ ADN (oilrNodeId lhs nk) | nk <- nodeIds rhs 
                                                   , not (nk `elem` nif) ]
        -- limit unnecessary edge deletion and insertion. TODO: check for parallel edges
        edgeInterface  = edgeIds lhs `intersect` edgeIds rhs
        edgeDeletions  = [ DEE (oilrEdgeId lhs ek) | ek <- edgeIds lhs
                                                   , not $ ek `elem` edgeInterface ]
        edgeInsertions = [ ADE (oilrEdgeId lhs ek) src tgt
                            | ek <- edgeIds rhs
                            , let src = oilrNodeId lhs (source ek)
                            , let tgt = oilrNodeId lhs (target ek)
                            , not $ ek `elem` edgeInterface ]
        newRoots      = [ RTN (oilrNodeId lhs nk)
                            | nk <- nodeIds rhs , isRoot rhs nk && not (isRoot lhs nk) ]
        unRoots       = [ URN (oilrNodeId lhs nk)
                            | nk <- nodeIds lhs , isRoot lhs nk && not (isRoot rhs nk) ]
        colourChanges = [ CON (oilrNodeId lhs nk)
                              (definiteLookup (colour rhs nk) colourIds)
                              | nk <- nodeIds rhs , colour lhs nk /= colour rhs nk  ]



oilrNodeId :: AstRuleGraph -> NodeKey -> GraphElemId
oilrNodeId g nk = (g, N nk)

oilrEdgeId :: AstRuleGraph -> EdgeKey -> GraphElemId
oilrEdgeId g ek = (g, E ek)



