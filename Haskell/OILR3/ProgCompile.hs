module OILR3.ProgCompile (compileProgram) where

import OILR3.Instructions

import GPSyntax
import Mapping

import Data.List
import Unsafe.Coerce
import Debug.Trace

data GraphElem = N NodeKey | E EdgeKey deriving (Show, Eq)
type GraphElemId = (AstRuleGraph, GraphElem)
type SemiOilrCode = [Instr GraphElemId GraphElemId]
type OilrCode = [Instr Int Int]

type NodeKey = String
type EdgeKey = (String, String, String)

type Interface = [String]

colourMapping :: Mapping Colour Dim
colourMapping = 
    [ (Uncoloured, Equ 0)
    , (Red       , Equ 1)
    , (Green     , Equ 2)
    , (Blue      , Equ 3)
    , (Grey      , Equ 4)
    , (Cyan      , GtE 0) ]


notImplemented n = error $ "Not implemented: " ++ show n


compileProgram :: [Flag] -> GPProgram -> [OilrCode]
compileProgram flags (Program ds) = map postprocess mappings
    where
        prog = map oilrCompileDeclaration ds
        mappings = map elemIdMapping prog


mergeTravs :: SemiOilrCode -> SemiOilrCode -> SemiOilrCode
mergeTravs nts ets = edgesToInstrs [] [] edges
    where
        edges  = [ (src, ed, tgt)
                    | src@(LUN sn _)  <- nts
                    , ed@(LUE e s t) <- ets
                    , tgt@(LUN tn _)  <- nts
                    , sn==s , tn==t ]

edgesToInstrs acc _ [] = reverse acc
edgesToInstrs acc seen ((LUN _ sp, LUE e s t, LUN _ tp):es) =
    case (s==t, s `elem` seen, t `elem` seen) of
        (_,     True,  True ) -> edgesToInstrs (LUE e s t:acc)          seen       es
        (True,  False, _    ) -> edgesToInstrs (LUE e s t:LUN s sp:acc) (s:seen)   es
        (False, False, True ) -> edgesToInstrs (XIE t e s:acc)          (s:seen)   es
        (False, True,  False) -> edgesToInstrs (XOE s e t:acc)          (t:seen)   es
        (False, False, False) -> edgesToInstrs (XOE s e t:LUN s sp:acc) (t:s:seen) es


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
        postprocessInstr (LUN n p)   = LUN (translate n) p
        postprocessInstr (LUE e s t) = LUE (translate e) (translate s) (translate t)
        postprocessInstr (XOE s e t) = XOE (translate s) (translate e) (translate t)
        postprocessInstr (XIE t e s) = XIE (translate t) (translate e) (translate s)
        postprocessInstr (NEC n1 n2) = NEC (translate n1) (translate n2)
        postprocessInstr i           = error $ show i ++ " is not implmented"

elemIdMapping :: SemiOilrCode -> (Mapping GraphElemId Int, SemiOilrCode)
elemIdMapping sois = ( zip (nub [ id | id <- concatMap extractId sois ]) [0,1..], sois )

extractId :: Instr a a -> [a]
extractId (ADN n)      = [n]
extractId (DEN n)      = [n]
extractId (RTN n)      = [n]
extractId (LUN n _)    = [n]
extractId (ADE e _ _)  = [e]
extractId (DEE e)      = [e]
extractId (LUE e _ _)  = [e]
extractId (XOE _ e t)  = [e, t]
extractId (XIE _ e s)  = [e, s]
extractId _            = []



-- -------------------------------------------------------------------
-- program OILR instruction generation
-- -------------------------------------------------------------------

{-   A handy AST reference...

data HostEdge = HostEdge NodeName NodeName HostLabel deriving Show
data AstHostGraph = AstHostGraph [HostNode] [HostEdge] deriving Show
data GPProgram = Program [Declaration] deriving Show

data Declaration = MainDecl Main
                 | ProcDecl Procedure
                 | AstRuleDecl AstRule
                 | RuleDecl Rule
     deriving Show

data Main = Main [Command] deriving Show

data Procedure = Procedure ProcName [Declaration] [Command] deriving Show

data Command = Block Block
             | IfStatement Block Block Block 
             | TryStatement Block Block Block
    deriving Show

data Block = ComSeq [Command]
           | LoopedComSeq [Command]
           | SimpleCommand SimpleCommand
           | ProgramOr Block Block      
    deriving (Show)
      
data SimpleCommand = RuleCall [RuleName]
                   | LoopedRuleCall [RuleName]
                   | ProcedureCall ProcName
                   | LoopedProcedureCall ProcName
                   | Skip
                   | Fail
    deriving Show

data Rule = Rule RuleName [Variable] (AstRuleGraph, AstRuleGraph) NodeInterface 
            EdgeInterface Condition deriving Show

data AstRule = AstRule RuleName [Variable] (AstRuleGraph, AstRuleGraph) 
               Condition  deriving Show
data AstRuleGraph = AstRuleGraph [RuleNode] [AstRuleEdge] deriving (Show,Eq)
data AstRuleEdge = AstRuleEdge EdgeName Bool NodeName NodeName RuleLabel deriving (Show, Eq)

data RuleNode = RuleNode NodeName Bool RuleLabel deriving (Show, Eq)
data Condition = NoCondition
               | TestInt VarName
               | TestChr VarName
               | TestStr VarName
               | TestAtom VarName
               | Edge NodeName NodeName (Maybe RuleLabel)
               | Eq GPList GPList
               | NEq GPList GPList
               | Greater RuleAtom RuleAtom
               | GreaterEq RuleAtom RuleAtom
               | Less RuleAtom RuleAtom
               | LessEq RuleAtom RuleAtom
               | Not Condition
               | Or Condition Condition
               | And Condition Condition
    deriving Show


-}


oilrCompileDeclaration :: Declaration -> SemiOilrCode
oilrCompileDeclaration (MainDecl m) = oilrCompileMain m
oilrCompileDeclaration (ProcDecl p) = oilrCompileProc p
oilrCompileDeclaration (AstRuleDecl r) = oilrCompileRule r

oilrCompileProc :: Procedure -> SemiOilrCode
oilrCompileProc (Procedure name ds cs) = (PRO name : concatMap oilrCompileCommand cs) ++ [END]

oilrCompileMain :: Main -> SemiOilrCode
oilrCompileMain (Main cs) = oilrCompileProc (Procedure "Main" [] cs)

oilrCompileCommand :: Command -> SemiOilrCode
oilrCompileCommand (Block b) = oilrCompileBlock b
oilrCompileCommand (IfStatement  cn th el) = notImplemented 2
oilrCompileCommand (TryStatement cn th el) = notImplemented 3

oilrCompileBlock :: Block -> SemiOilrCode
oilrCompileBlock (ComSeq cs)       = notImplemented 4
oilrCompileBlock (LoopedComSeq cs) = notImplemented 5
oilrCompileBlock (SimpleCommand s) = oilrCompileSimple s
oilrCompileBlock (ProgramOr a b)   = notImplemented 6

oilrCompileSimple :: SimpleCommand -> SemiOilrCode
oilrCompileSimple (RuleCall      [r]) = [ CAL r ]
oilrCompileSimple (LoopedRuleCall [r]) = [ ALP r ]
oilrCompileSimple (RuleCall       rs) = notImplemented 7 -- non-deterministic choice(?)
oilrCompileSimple (LoopedRuleCall rs) = notImplemented 8
oilrCompileSimple (ProcedureCall       p) = [ CAL p ]
oilrCompileSimple (LoopedProcedureCall p) = [ ALP p ]
oilrCompileSimple Skip   = [ TRU , RET ]
oilrCompileSimple Fail   = [ FLS , RET ]



-- -------------------------------------------------------------------
-- rule compilation is complex...
-- -------------------------------------------------------------------


nodeIds :: AstRuleGraph -> Interface
nodeIds (AstRuleGraph ns _) = map (\(RuleNode id _ _) -> id) ns

edgeIds :: AstRuleGraph -> [EdgeKey]
edgeIds (AstRuleGraph _ es) = map (\(AstRuleEdge id bidi src tgt _) -> (id, src, tgt)) es

rootNodes :: AstRuleGraph -> [NodeKey]
rootNodes (AstRuleGraph ns _) = [ nk | (RuleNode nk root _) <- ns , root == True ]

nodeColours :: AstRuleGraph -> Mapping NodeKey Colour
nodeColours (AstRuleGraph ns _) = [ (nk, c) | (RuleNode nk _ (RuleLabel _ c)) <- ns ]


source :: EdgeKey -> NodeKey
source (_, nk, _) = nk

target :: EdgeKey -> NodeKey
target (_, _, nk) = nk

inDegree :: AstRuleGraph -> NodeKey -> Int
inDegree g nk = length [ ek | ek <- edgeIds g , target ek == nk ]

outDegree :: AstRuleGraph -> NodeKey -> Int
outDegree g nk = length [ ek | ek <- edgeIds g , source ek == nk ]

loopCount :: AstRuleGraph -> NodeKey -> Int
loopCount g nk = length [ ek | ek <- edgeIds g , source ek == nk && target ek == nk ]

isRoot :: AstRuleGraph -> NodeKey -> Bool
isRoot g nk = nk `elem` rootNodes g

colour :: AstRuleGraph -> NodeKey -> Colour
colour g nk = definiteLookup nk $ nodeColours g

-- TODO: special handling for bidi edges!
oilrCompileRule :: AstRule -> SemiOilrCode
oilrCompileRule r@(AstRule name _ (lhs, rhs) cond) = ( [RUL name] ++ body ++ [UBA, END] )
    where
        nif  = nodeIds lhs `intersect` nodeIds rhs
        body = oilrCompileLhs lhs nif ++ oilrCompileCondition lhs cond ++ oilrCompileRhs lhs rhs nif

-- Make sure the most constrained nodes are looked for first
oilrSortNodeLookups :: SemiOilrCode -> SemiOilrCode
oilrSortNodeLookups is = reverse $ sortBy mostConstrained is
    where
        mostConstrained (LUN _ p1) (LUN _ p2) = comparePreds p1 p2

oilrSortEdgeLookups :: SemiOilrCode -> SemiOilrCode
oilrSortEdgeLookups is = interesting ++ uninteresting
    where
        (interesting, uninteresting) = partition isInteresting is
        isInteresting (LUE _ s t) | s == t    =  True
                                  | otherwise = 
            ( length $ filter (\(LUE e' s' t') -> s==s' && t==t' || s==t' && t==s') is ) > 1

-- NOTE: haskell's standard compare on n-tuples doesn't give good results
-- so I've developed a custom points-based approach
comparePreds :: Pred -> Pred -> Ordering
comparePreds p1 p2 = compare (predToWeight p1) (predToWeight p2)

predToWeight :: Pred -> Int
predToWeight (c, o, i, l, r) = 4 * valueForDim r + 2*valueForDim l + (sum $ map valueForDim [o,i])

valueForDim :: Dim -> Int
valueForDim (GtE n) = n
valueForDim (Equ n) = (n+1)*2

-- TODO: needs work. Currently issues "n2 e12 e23 n1 n3" but ideally should be "n2 e12 n1 e23 n3"
{- oilrInterleaveEdges :: SemiOilrCode -> SemiOilrCode -> SemiOilrCode -> SemiOilrCode
oilrInterleaveEdges _ es ns = ns ++ es  -- TODO: hack other cases not reachable
oilrInterleaveEdges acc es [] = reverse acc ++ es
oilrInterleaveEdges acc es (n@(LUN id _):ns) = oilrInterleaveEdges (nes ++ n:acc) es' ns
    where
        (nes, es') = partition (edgeFor id) es
        edgeFor id (LUE _ a b) = a == id || b == id
        insertEdgeBeforeNode = notImplemented 20 -}

oilrCompileLhs :: AstRuleGraph -> Interface -> SemiOilrCode
oilrCompileLhs lhs nif = code
    where
        code   = mergeTravs nTravs eTravs
        eTravs = oilrSortEdgeLookups $ map compileEdge (edgeIds lhs)
        nTravs = oilrSortNodeLookups $ map compileNode (nodeIds lhs)
        compileNode nk = cn
             where
                cn = if nk `elem` nif
                        then LUN (oilrNodeId lhs nk) (c, GtE o, GtE i, GtE l, r)
                        else LUN (oilrNodeId lhs nk) (c, Equ o, Equ i, Equ l, r)
                o = outDegree lhs nk - l
                i = inDegree lhs nk - l
                l = loopCount lhs nk
                r = if isRoot lhs nk then Equ 1 else GtE 0
                c = definiteLookup (colour lhs nk) colourMapping
        compileEdge ek = LUE (oilrEdgeId lhs ek) (oilrNodeId lhs $ source ek) (oilrNodeId lhs $ target ek)

oilrCompileCondition :: AstRuleGraph -> Condition -> SemiOilrCode
oilrCompileCondition _ NoCondition = []
oilrCompileCondition lhs (Not (Edge a b Nothing)) = [NEC (oilrNodeId lhs a) (oilrNodeId lhs b)]
oilrCompileCondition _ _ = notImplemented 12


-- Note that when making unique IDs from the RHS we _always_ use the LHS graph, not the RHS.
-- The inclusion of the graph in the unique ID is purely to identify elements that belong to
-- same rule, and should not be used for any other purpose.
oilrCompileRhs :: AstRuleGraph -> AstRuleGraph -> Interface -> SemiOilrCode
oilrCompileRhs lhs rhs nif = edgeDeletions ++ nodeDeletions ++ nodeInsertions ++ edgeInsertions
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



oilrNodeId :: AstRuleGraph -> NodeKey -> GraphElemId
oilrNodeId g nk = (g, N nk)

oilrEdgeId :: AstRuleGraph -> EdgeKey -> GraphElemId
oilrEdgeId g ek = (g, E ek)



