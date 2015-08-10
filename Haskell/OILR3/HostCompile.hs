module OILR3.HostCompile (compileHostGraph, compileProgram) where

import OILR3.Instructions

import GPSyntax
import Graph
import Mapping

import Debug.Trace
import Unsafe.Coerce
import Data.List


data GraphElem = N NodeKey | E EdgeKey deriving (Eq, Ord, Show)
type GraphElemId = (RuleGraph, GraphElem)
type SemiOilrCode = [Instr GraphElemId GraphElemId]
type OilrCode = [Instr Int Int]




notImplemented n = error $ "Not implemented: " ++ show n


compileHostGraph :: HostGraph -> OilrCode
compileHostGraph g = nodes g ++ edges g

compileProgram :: GPProgram -> ([OilrCode], Int)
compileProgram (Program ds) = (map (postprocess mapping) prog , length mapping)
    where
        prog = map oilrCompileDeclaration ds
        mapping = elemIdMapping $ concat prog


postprocess :: Mapping GraphElemId Int -> SemiOilrCode -> OilrCode
postprocess mapping sois = map postprocessInstr sois
    where
        translate :: GraphElemId -> Int
        translate id = definiteLookup id mapping

        postprocessInstr :: Instr GraphElemId GraphElemId -> Instr Int Int
        postprocessInstr (ADN n)     = ADN $ translate n
        postprocessInstr (ADE e s t) = ADE (translate e) (translate s) (translate t)
        postprocessInstr (DEN n)     = DEN $ translate n
        postprocessInstr (DEE e)     = DEE $ translate e
        postprocessInstr (RTN n)     = RTN $ translate n
        postprocessInstr (LUN n p)   = LUN (translate n) p
        postprocessInstr (LUE e s t) = LUE (translate e) (translate s) (translate t)
        -- these below shouldn't be in the instruction stream at this stage -- they're 
        -- only created by optimisations, however they're handled here for type-safety's
        -- sake
        postprocessInstr (XOE e s)   = XOE (translate e) (translate s)
        postprocessInstr (XIE e t)   = XOE (translate e) (translate t)
        postprocessInstr (XSN n e)   = XOE (translate n) (translate e)
        postprocessInstr (XTN n e)   = XOE (translate n) (translate e)
        postprocessInstr (ORB n)     = ORB $ translate n
        postprocessInstr (CRS n p)   = CRS (translate n) p
        -- WARNING: HERE BE DRAGONS. Haskell's type system won't do implicit 
        -- conversion between the non-parameterised elements of the parameterised
        -- type Isntr a b. unsafeCoerce allows this conversion by sidestepping 
        -- the type system entirely! If any new parameterised elements are introduced
        -- in Instr a b they _must_ be handled above. If they aren't, good luck
        -- debugging the results! Don't say you weren't warned.
        postprocessInstr soi         = unsafeCoerce soi

elemIdMapping :: SemiOilrCode -> Mapping GraphElemId Int
elemIdMapping sois = zip (nub [ id | id <- concatMap extractId sois ]) [0,1..]

extractId :: Instr a a -> [a]
extractId (ADN n)      = [n]
extractId (DEN n)      = [n]
extractId (RTN n)      = [n]
extractId (LUN n _)    = [n]
extractId (ADE e _ _)  = [e]
extractId (DEE e)      = [e]
extractId (LUE e _ _)  = [e]
extractId _            = []




-- -------------------------------------------------------------------
-- host graph OILR instruction generation
-- -------------------------------------------------------------------

nodes :: HostGraph -> OilrCode
nodes g = concatMap node $ allNodes g
    where
        node (n, HostNode _ root (HostLabel [] Uncoloured)) = ADN (nodeNumber n) : (if root then RTN (nodeNumber n) : [] else [])


edges :: HostGraph -> OilrCode
edges g = map edge $ allEdges g
    where
        edge (e, _) = ADE (edgeNumber e) (nodeNumber $ source e) (nodeNumber $ target e)


-- -------------------------------------------------------------------
-- program OILR instruction generation
-- -------------------------------------------------------------------

{-   A handy AST reference...

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

data Rule = Rule RuleName [Variable] (RuleGraph, RuleGraph) NodeInterface 
            EdgeInterface Condition deriving Show

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
oilrCompileDeclaration (RuleDecl r) = oilrCompileRule r

oilrCompileMain :: Main -> SemiOilrCode
oilrCompileMain (Main cs) = (DEF "Main" : concatMap oilrCompileCommand cs) ++ [END]

oilrCompileProc :: Procedure -> SemiOilrCode
oilrCompileProc (Procedure name ds cs) = notImplemented 1

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

oilrNodeId :: RuleGraph -> NodeKey -> GraphElemId
oilrNodeId g nk = (g, N nk)

oilrEdgeId :: RuleGraph -> EdgeKey -> GraphElemId
oilrEdgeId g ek = (g, E ek)

-- TODO: special handling for bidi edges!
oilrCompileRule :: Rule -> SemiOilrCode
oilrCompileRule r@(Rule name _ (lhs, rhs) nif eif _) = ( [DEF name] ++ body ++ [END] )
    where
        body = oilrCompileLhs lhs nif ++ oilrCompileRhs lhs rhs nif

-- Make sure the most constrained nodes are looked for first
oilrSortNodeLookups :: SemiOilrCode -> SemiOilrCode
oilrSortNodeLookups is = reverse $ sortBy mostConstrained is
    where
        mostConstrained (LUN _ p1) (LUN _ p2) = compare p1 p2

-- TODO: needs work. Currently issues "n2 e12 e23 n1 n3" but ideally should be "n2 e12 n1 e23 n3"
oilrInterleaveEdges :: SemiOilrCode -> SemiOilrCode -> SemiOilrCode -> SemiOilrCode
oilrInterleaveEdges acc es [] = reverse acc ++ es
oilrInterleaveEdges acc es (n@(LUN id _):ns) = oilrInterleaveEdges (nes ++ n:acc) es' ns
    where
        (nes, es') = partition (edgeFor id) es
        edgeFor id (LUE _ a b) = a == id || b == id
        insertEdgeBeforeNode = notImplemented 20

oilrCompileLhs :: RuleGraph -> NodeInterface -> SemiOilrCode
oilrCompileLhs lhs nif = oilrInterleaveEdges [] (map compileEdge (allEdgeKeys lhs)) $ oilrSortNodeLookups ( map compileNode (allNodeKeys lhs) )
    where
        compileNode nk = cn
             where
                cn = if nk `elem` dom nif
                        then LUN (oilrNodeId lhs nk) (GtE o, GtE i, GtE l, r)
                        else LUN (oilrNodeId lhs nk) (Equ o, Equ i, Equ l, r)
                o = outdegree lhs nk - l
                i = indegree lhs nk - l
                l = length $ joiningEdges lhs nk nk
                r = GtE 0 -- TODO!
        compileEdge ek = LUE (oilrEdgeId lhs ek) (oilrNodeId lhs $ source ek) (oilrNodeId lhs $ target ek)

oilrCompileRhs :: RuleGraph -> RuleGraph -> NodeInterface -> SemiOilrCode
oilrCompileRhs lhs rhs nif = edgeDeletions ++ nodeDeletions ++ nodeInsertions ++ edgeInsertions
    where
        edgeDeletions  = [ DEE (oilrEdgeId lhs ek) | ek <- allEdgeKeys lhs ]
        nodeDeletions  = [ DEN (oilrNodeId lhs nk) | nk <- allNodeKeys lhs , not (nk `elem` dom nif) ]
        nodeInsertions = [ ADN (oilrNodeId rhs nk) | nk <- allNodeKeys rhs , not (nk `elem` rng nif) ]
        edgeInsertions = [ ADE (oilrEdgeId rhs ek) src tgt
                            | ek <- allEdgeKeys rhs
                            , let src = oilrNodeId rhs (source ek)
                            , let tgt = oilrNodeId rhs (target ek) ]


oilrCompileCondition NoCondition = []
oilrCompileCondition _ = notImplemented 12

