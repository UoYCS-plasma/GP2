module OILR3.HostCompile (compileHostGraph, compileProgram) where

import OILR3.Instructions

import GPSyntax
import Graph
import Mapping (dom, rng)

import Debug.Trace
import Unsafe.Coerce
import Data.List


type SemiOilrCode = [Instr NodeKey EdgeKey]
type OilrCode = [Instr Int Int]

notImplemented n = error $ "Not implemented: " ++ show n


compileHostGraph :: HostGraph -> OilrCode
compileHostGraph g = postprocess $ nodes g ++ edges g

compileProgram :: GPProgram -> [OilrCode]
compileProgram (Program ds) = map postprocess $ map oilrCompileDeclaration ds


postprocess :: SemiOilrCode -> OilrCode
postprocess sois = map postprocessInstr sois
    where
        postprocessInstr (ADN n)     = ADN $ nodeNumber n
        postprocessInstr (ADE e s t) = ADE (edgeNumber e) (nodeNumber s) (nodeNumber t)
        postprocessInstr (DEN n)     = DEN $ nodeNumber n
        postprocessInstr (DEE e)     = DEE $ edgeNumber e
        postprocessInstr (RTN n)     = RTN $ nodeNumber n
        postprocessInstr (LUN n p)   = LUN (nodeNumber n) p
        postprocessInstr (LUE e s t) = LUE (edgeNumber e) (nodeNumber s) (nodeNumber t)
        -- these below shouldn't be in the instruction stream at this stage -- they're 
        -- only created by optimisations, however they're handled here for type-safety's
        -- sake
        postprocessInstr (XOE e s)   = XOE (edgeNumber e) (nodeNumber s)
        postprocessInstr (XIE e t)   = XOE (edgeNumber e) (nodeNumber t)
        postprocessInstr (XSN n e)   = XOE (nodeNumber n) (edgeNumber e)
        postprocessInstr (XTN n e)   = XOE (nodeNumber n) (edgeNumber e)
        postprocessInstr (ORB n)     = ORB $ nodeNumber n
        -- WARNING: HERE BE DRAGONS. Haskell's type system won't do implicit 
        -- conversion between the non-parameterised elements of the parameterised
        -- type Isntr a b. unsafeCoerce allows this conversion by sidestepping 
        -- the type system entirely! If any new parameterised elements are introduced
        -- in Instr a b they _must_ be handled above. If they aren't, good luck
        -- debugging the results! Don't say you weren't warned.
        postprocessInstr soi         = unsafeCoerce soi


-- -------------------------------------------------------------------
-- host graph OILR instruction generation
-- -------------------------------------------------------------------

nodes :: HostGraph -> SemiOilrCode
nodes g = concatMap node $ allNodes g
    where
        node (n, HostNode _ root (HostLabel [] Uncoloured)) = ADN n : (if root then RTN n : [] else [])


edges :: HostGraph -> SemiOilrCode
edges g = map edge $ allEdges g
    where
        edge (e, _) = ADE e (source e) (target e)


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

data OilrRuleProp = NodesAdded | NodesDeleted | EdgesAdded | EdgesDeleted deriving Show


-- Return the lists of (deleted, created) node ids

analyseNodeInterface :: Rule -> ([NodeKey], [NodeKey])
analyseNodeInterface (Rule _ _ (lhs, rhs) nif _ _) = 
    ( [ lnk | lnk <- allNodeKeys lhs , not $ lnk `elem` dom nif ]
    , [ rnk | rnk <- allNodeKeys rhs , not $ rnk `elem` rng nif ] )
    where
        lhsInterfaceNodes = dom nif
        rhsInterfaceNodes = rng nif

-- WARNING: EdgeInterface doesn't do what it says on the tin! It is only used
-- to track bidi rule edges, not as an actual interface. We'll have to remove
-- unneeded edge deletion and creation rules
analyseRule :: Rule -> ([NodeKey], [NodeKey])
analyseRule r = analyseNodeInterface r


-- TODO: special handling for bidi edges!
oilrCompileRule :: Rule -> SemiOilrCode
oilrCompileRule r@(Rule name _ (lhs, rhs) nif eif _) = ( [DEF name] ++ body ++ [END] )
    where
        body = oilrCompileLhs lhs nif ++ oilrCompileRhs lhs rhs nif

oilrSortNodeLookups :: SemiOilrCode -> SemiOilrCode
oilrSortNodeLookups is = reverse $ sortBy mostConstrained is
    where
        mostConstrained (LUN _ p1) (LUN _ p2) = compare p1 p2

oilrInterleaveEdges :: SemiOilrCode -> SemiOilrCode -> SemiOilrCode -> SemiOilrCode
oilrInterleaveEdges acc es [] = reverse acc ++ es
oilrInterleaveEdges acc es (n@(LUN id _):ns) = oilrInterleaveEdges (nes ++ n:acc) es' ns
    where
        (nes, es') = partition (edgeFor id) es
        edgeFor id (LUE _ a b) = a == id || b == id

oilrCompileLhs :: RuleGraph -> NodeInterface -> SemiOilrCode
oilrCompileLhs lhs nif = oilrInterleaveEdges [] (map compileEdge (allEdgeKeys lhs)) $ oilrSortNodeLookups ( map compileNode (allNodeKeys lhs) )
    where
        compileNode nk = cn
             where
                cn = if nk `elem` dom nif
                        then LUN nk (GtE o, GtE i, GtE l, r)
                        else LUN nk (Equ o, Equ i, Equ l, r)
                o = outdegree lhs nk - l
                i = indegree lhs nk - l
                l = length $ joiningEdges lhs nk nk
                r = GtE 0 -- TODO!
        compileEdge ek = LUE ek (source ek) (target ek)

oilrCompileRhs :: RuleGraph -> RuleGraph -> NodeInterface -> SemiOilrCode
oilrCompileRhs lhs rhs nif = edgeDeletions ++ nodeDeletions ++ nodeInsertions ++ edgeInsertions
    where
        edgeDeletions  = [ DEE ek | ek <- allEdgeKeys lhs ]
        nodeDeletions  = [ DEN nk | nk <- allNodeKeys lhs , not (nk `elem` dom nif) ]
        nodeInsertions = [ ADN nk | nk <- allNodeKeys rhs , not (nk `elem` rng nif) ]
        edgeInsertions = [ ADE ek src tgt
                            | ek <- allEdgeKeys rhs
                            , let src = source ek
                            , let tgt = target ek ]


oilrCompileCondition NoCondition = []
oilrCompileCondition _ = notImplemented 12

