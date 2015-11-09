module OILR3.IR where

import Data.List

import GPSyntax
import Graph

import OILR3.Instructions


type Id = String

data OilrMod a = Same a  |  Change a a   deriving (Show, Eq)

data IRLabel = IRInt Int
             | IRVar Id
             | IREmpty
     deriving (Show, Eq)

data OilrElem = IRNode  Id  Colour  IRLabel  Bool
              | IREdge  Id  Colour  IRLabel  Bool  Id Id
              | IRNac   OilrElem
              | IRNothing
     deriving (Show, Eq)

type OilrRule = [OilrMod OilrElem]

data OilrIR   = IRProc Id OilrExpr
              | IRRule Id OilrRule
     deriving (Show, Eq)

data OilrExpr = IRSeq [OilrExpr]
              | IRSet [Id]
              | IRBrn OilrExpr OilrExpr
              | IRTrn OilrExpr   -- transaction that rolls-back if OilrExpr fails
              | IRDsc OilrExpr -- "discard" -- transaction that always rolls-back
              | IRCal Id | IRLoo OilrExpr
              | IRTru | IRFls
     deriving (Show, Eq)

makeIR :: [Flag] -> GPProgram -> [OilrIR]
makeIR flags (Program ds) = map declIR ds

declIR :: Declaration -> OilrIR
declIR (Main cs)                          = declIR (Proc "Main" [] cs)
declIR (AstRuleDecl r@(AstRule id _ _ _)) = IRRule id $ ruleDeclIR r
declIR p@(Proc id ds cs)                  = IRProc id $ exprIR (Sequence cs) -- TODO: ???

exprIR :: Expr -> OilrExpr
exprIR (RuleSet rs)            = IRSet rs
exprIR (Sequence es)           = IRTrn (IRSeq $ map exprIR es)
exprIR (IfStatement cn th el)  = IRSeq [IRDsc (exprIR cn), IRBrn (exprIR th) (exprIR el)]
exprIR (TryStatement cn th el) = IRSeq [IRTrn (exprIR cn), IRBrn (exprIR th) (exprIR el)]
exprIR (ProgramOr a b)          = error "Not implemented"
exprIR (ProcedureCall p)        = IRCal p
exprIR (Looped (RuleSet rs))    = IRLoo (IRSet rs)
exprIR (Looped (ProcedureCall p))=IRLoo (IRCal p)
exprIR (Looped s@(Sequence _))  = IRLoo (exprIR s)
exprIR Skip                     = IRTru
exprIR Fail                     = IRFls


ruleDeclIR :: AstRule -> OilrRule
ruleDeclIR  = error ""

ruleGraphIR :: AstRuleGraph -> AstRuleGraph -> OilrRule
ruleGraphIR lhs rhs = nodes ++ edges
    where nodes = map irNode $ allNodePairs lhs rhs
          edges = makeIREdges lhs rhs

-- Node mangling

allNodePairs :: AstRuleGraph -> AstRuleGraph -> [(Maybe RuleNode, Maybe RuleNode)]
allNodePairs lhs rhs = [ (findNode lhs id, findNode rhs id) | id <- nids ]
    where nids = allNodeIds lhs rhs

findNode :: AstRuleGraph -> Id -> Maybe RuleNode
findNode (AstRuleGraph ns _) id =
    case [ rn | rn@(RuleNode ident _ _) <- ns , id==ident] of
    []  -> Nothing
    [n] -> Just n
    _   -> error "Found several nodes with the same id!"

allNodeIds :: AstRuleGraph -> AstRuleGraph -> [Id]
allNodeIds (AstRuleGraph lns _) (AstRuleGraph rns _) =
    [ lid | (RuleNode lid _ _) <- lns ] `union` [ rid | (RuleNode rid _ _) <- rns ]

-- Edge mangling

-- AstRuleEdge EdgeName Bool NodeName NodeName RuleLabel

packEdges :: OilrRule -> [OilrElem] -> [OilrElem] -> OilrRule
packEdges acc [] []     = reverse acc
packEdges acc [] (r:rs) =        packEdges (Change IRNothing r:acc) [] rs
packEdges acc (l:ls) [] =        packEdges (Change l IRNothing:acc) ls []
packEdges acc (l:ls) rs = case partition (==l) rs of
    (r:_, _) ->                  packEdges (Same l            :acc) ls (delete r rs)
    ([] , _) -> case partition (irEdgeSimilarity l) rs of
                    (r:_, _)  -> packEdges (Change l r        :acc) ls (delete r rs)
                    ([],  _)  -> packEdges (Change l IRNothing:acc) ls rs
    -- TODO: make this more efficient by distinguishing structural changes from textual

irEdgeSimilarity :: OilrElem -> OilrElem -> Bool
irEdgeSimilarity (IREdge _ _ _ True  lsrc ltgt) (IREdge _ _ _ bi rsrc rtgt)
    | (lsrc == rsrc && ltgt == rtgt ) = bi
    | (lsrc == rtgt && ltgt == rsrc ) = bi
    | otherwise                       = False
irEdgeSimilarity (IREdge _ _ _ False lsrc ltgt) (IREdge _ _ _ False rsrc rtgt)
    | lsrc == rsrc && ltgt == rtgt  = True
    | otherwise                     = False


makeIREdges :: AstRuleGraph -> AstRuleGraph -> OilrRule
makeIREdges (AstRuleGraph _ les) (AstRuleGraph _ res) =
    packEdges [] [ makeIREdge e | e <- les ]
                 [ makeIREdge e | e <- res ]

-- TODO: edge id?
makeIREdge :: AstRuleEdge -> OilrElem
makeIREdge (AstRuleEdge _ bidi src tgt (RuleLabel l c)) =
    IREdge "" c (makeIRLabel l) bidi src tgt

edgeEquality :: AstRuleEdge -> AstRuleEdge -> Bool
edgeEquality (AstRuleEdge _ False lsrc ltgt ll) (AstRuleEdge _ False rsrc rtgt rl) = 
    lsrc == rsrc && ltgt == rtgt && ll == rl
edgeEquality (AstRuleEdge _ True _ _ _) (AstRuleEdge _ False _ _ _) = False
edgeEquality (AstRuleEdge _ True lsrc ltgt ll) (AstRuleEdge _ True rsrc rtgt rl) = 
    ( lsrc == rsrc && ltgt == rtgt || lsrc == rtgt && rsrc == ltgt ) && ll == rl

-- IR creation

irNode :: (Maybe RuleNode, Maybe RuleNode) -> OilrMod OilrElem
irNode = irElem makeIRNode (==)

irEdge :: (Maybe AstRuleEdge, Maybe AstRuleEdge) -> OilrMod OilrElem
irEdge = irElem makeIREdge edgeEquality

irElem :: (a -> OilrElem) -> (a -> a -> Bool) -> (Maybe a, Maybe a) -> OilrMod OilrElem
irElem mkElem _   (Just l, Nothing) = Change (mkElem l) IRNothing
irElem mkElem _   (Nothing, Just r) = Change IRNothing (mkElem r)
irElem mkElem eql (Just l,  Just r)
            | l `eql` r  =  Same   (mkElem l)
            | otherwise  =  Change (mkElem l) (mkElem r)

makeIRNode :: RuleNode -> OilrElem
makeIRNode (RuleNode id root (RuleLabel l c)) = IRNode id c i root
    where i = makeIRLabel l

makeIRLabel :: [RuleAtom] -> IRLabel
makeIRLabel []                  = IREmpty
makeIRLabel [Val (Int i)]       = IRInt i
makeIRLabel [Val v]             = error $ "Unsupported literal value: " ++ show v
makeIRLabel [Var (var, IntVar)] = IRVar var
makeIRLabel [Var (var, t)]      = error $ var ++ " is of unsupported type: " ++ show t
makeIRLabel [atom]              = error $ "Unsupported atom: " ++ show atom
makeIRLabel (x:xs)              = error "List type labels are not supported"

