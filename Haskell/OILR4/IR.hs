module OILR4.IR where

import Data.List

import GPSyntax
import Graph
import Mapping

type Id = String

data OilrMod = Create OilrElem
             | Check OilrElem
             | Same OilrElem
             | Change OilrElem OilrElem
             | Delete OilrElem
    deriving (Show, Eq)

instance Ord OilrMod where
    a `compare` b = constrainedness a `compare` constrainedness b

data IRLabel = IRInt Int
             | IRVar Id  -- Integer variable
             | IRLst Id  -- GP 2 list. preserved but not manipulated
             | IRAny
             | IREmpty
     deriving (Show, Eq)

data Sig = Sig { outDeg      :: Int
               , inDeg       :: Int
               , loopDeg     :: Int
               , rootDeg     :: Bool }
         | NoSig
     deriving (Show, Eq) 

{- instance Ord Sig where
    NoSig            `compare` NoSig             = EQ
    NoSig            `compare` (Sig _ _ _ _)     = LT
    (Sig _ _ _ _)    `compare` NoSig             = GT
    (Sig _ _ _ True) `compare` (Sig _ _ _ False) = GT
    (Sig _ _ _ False) `compare` (Sig _ _ _ True) = LT
    (Sig o1 i1 l1 _) `compare` (Sig o2 i2 l2 _)  = (o1+i1+l1) `compare` (o2+i2+l2)
    x `compare` y = error $ "Bugger: " ++ show [x,y] -}

data OilrElem = IRNode  Id  Colour  IRLabel  Sig
              | IREdge  Id  Colour  IRLabel  Bool  Id Id
              | IREql   IRLabel IRLabel
              | IRNot   OilrElem
              | IRNothing
     deriving (Show, Eq)
{- instance Ord OilrElem where
    (IRNode _ _ _ a)     `compare` (IRNode _ _ _ b)     = a `compare` b
    (IRNode _ _ _ _)     `compare` _                    = GT
    _                    `compare` (IRNode _ _ _ _)     = GT
    (IREdge _ _ _ _ _ _) `compare` (IREdge _ _ _ _ _ _) = EQ
    a `compare` b = error $ "Arse: " ++ show [a,b] -}

type OilrRule = [OilrMod]

data OilrIR   = IRProc Id OilrExpr
              | IRRule Id OilrRule
     deriving (Show, Eq)
type OilrProg = [OilrIR]

data OilrExpr = IRSeqn [OilrExpr]
              | IRIf   OilrExpr OilrExpr OilrExpr
              | IRTry  OilrExpr OilrExpr OilrExpr
              | IRTrns OilrExpr   -- transaction that rolls-back if OilrExpr fails
              | IRRuleSet [Id] | IRCall Id | IRLoop OilrExpr
              | IRTrue | IRFals
     deriving (Show, Eq)

type Spc = [Sig]


constrainedness :: OilrMod -> Int
constrainedness (Delete el)   = eCon el * 10
constrainedness (Change el _) = eCon el
constrainedness _ = 0

eCon :: OilrElem -> Int
eCon (IRNode _ c l sig) = lCon l + cCon c + sCon sig
eCon _                  = 0

lCon :: IRLabel -> Int
lCon (IRVar _) = 0
lCon IRAny     = 0
lCon IREmpty   = 1
lCon (IRInt _) = 1

cCon :: Colour -> Int
cCon Any     = 0
cCon _       = 1

sCon :: Sig -> Int
sCon (Sig o i l r)
    | r         = o+i+(2*l)+1
    | otherwise = o+i+(2*l)
sCon NoSig = 0

-- IR pretty-printer
prettyIR :: OilrProg -> String
prettyIR pr = concatMap prettyDecl pr
    where  prettyDecl (IRProc id e) = concat [ "\nIRProc ", id, " (\n\t", prettyExpr e, "\n)"]
           prettyDecl (IRRule id es) = concat [ "\nIRRule ", id, " [\n\t", prettyRule es, "\n]"]
           prettyExpr = show
           prettyRule es = intercalate "\n\t" $ map show es

-- IR compilation wrapper function
makeIR :: GPProgram -> OilrProg
makeIR (Program ds) = map declIR ds

-- Compile a proc or rule declaration to OILR IR
declIR :: Declaration -> OilrIR
declIR (Main cs)                          = declIR (Proc "Main" [] cs)
declIR p@(Proc id ds [c])                 = IRProc id $ exprIR c
declIR p@(Proc id ds cs)                  = IRProc id $ exprIR (Sequence cs)
declIR (AstRuleDecl r@(AstRule id _ _ _)) = IRRule id $ ruleIR r

-- Compile a procedure expression to OILR IR
exprIR :: Expr -> OilrExpr
exprIR (RuleSet rs)            = IRRuleSet rs
exprIR (Sequence es)           = IRTrns (IRSeqn $ map exprIR es)
exprIR (IfStatement cn th el)  = IRIf (exprIR cn) (exprIR th) (exprIR el)
--        IRSeqn [IRDscd (exprIR cn), IRBran (exprIR th) (exprIR el)]
exprIR (TryStatement cn th el) = IRTry (exprIR cn) (exprIR th) (exprIR el)
--        IRSeqn [IRTrns (exprIR cn), IRBran (exprIR th) (exprIR el)]
exprIR (ProgramOr a b)          = error "Not implemented"
exprIR (ProcedureCall p)        = IRCall p
exprIR (Looped (RuleSet rs))    = IRLoop (IRRuleSet rs)
exprIR (Looped (ProcedureCall p))=IRLoop (IRCall p)
exprIR (Looped s@(Sequence _))  = IRLoop (exprIR s)
exprIR Skip                     = IRTrue
exprIR Fail                     = IRFals


-- Compile a rule body to OILR IR
-- data AstRule = AstRule RuleName [Variable] (AstRuleGraph, AstRuleGraph) Condition
ruleIR :: AstRule -> OilrRule
ruleIR (AstRule id vs (lhs, rhs) cond) = ruleGraphIR lhs rhs ++ irConds
    where irConds = case ruleCondIR cond of
                        IRNothing              -> []
                        e@(IREdge _ _ _ _ _ _) -> [Same e]
                        c                      -> [Check c]

-- Compile a pair of GP2 graphs to an IR rule
ruleGraphIR :: AstRuleGraph -> AstRuleGraph -> OilrRule
ruleGraphIR lhs rhs = nodes ++ edges
    where nodes = map (irNode sigs) $ allNodePairs lhs rhs
          edges = makeIREdges lhs rhs
          sigs  = makeSigs lhs

makeSigs :: AstRuleGraph -> Mapping Id Sig
makeSigs (AstRuleGraph ns es) = map makeSig ns
    where makeSig :: RuleNode -> (Id, Sig)
          makeSig (RuleNode id r _) = (id, Sig (nO id es) (nI id es) (nL id es) r) 
          nO :: Id -> [AstRuleEdge] -> Int
          nO id es = length $ filter (\(AstRuleEdge _ _ src tgt _) -> src == id && tgt /= id) es
          nI :: Id -> [AstRuleEdge] -> Int
          nI id es = length $ filter (\(AstRuleEdge _ _ src tgt _) -> tgt == id && src /= id) es
          nL :: Id -> [AstRuleEdge] -> Int
          nL id es = length $ filter (\(AstRuleEdge _ _ src tgt _) -> src == id && tgt == id) es


-- Make sure conditional rules are compiled correctly
ruleCondIR :: Condition -> OilrElem
ruleCondIR NoCondition         = IRNothing
ruleCondIR (Not c)             = IRNot $ ruleCondIR c
ruleCondIR (Edge s t Nothing)                = IREdge "" Any IRAny           False s t
ruleCondIR (Edge s t (Just (RuleLabel l c))) = IREdge "" c   (makeIRLabel l) False s t
ruleCondIR (Eq as bs)          = IREql (makeIRLabel as) (makeIRLabel bs)
ruleCondIR c = error $ "Unsupported conditional: " ++ show c

---------------------------------------------------------------------
-- Node mangling...
---------------------------------------------------------------------

-- Match-up LHS and RHS nodes
allNodePairs :: AstRuleGraph -> AstRuleGraph -> [(Maybe RuleNode, Maybe RuleNode)]
allNodePairs lhs rhs = [ (findNode lhs id, findNode rhs id) | id <- nids ]
    where nids = allNodeIds lhs rhs

-- Get a node by its Id
findNode :: AstRuleGraph -> Id -> Maybe RuleNode
findNode (AstRuleGraph ns _) id =
    case [ rn | rn@(RuleNode ident _ _) <- ns , id==ident] of
    []  -> Nothing
    [n] -> Just n
    _   -> error "Found several nodes with the same id!"

-- Get the set of all node ids found in either side of the rule
allNodeIds :: AstRuleGraph -> AstRuleGraph -> [Id]
allNodeIds (AstRuleGraph lns _) (AstRuleGraph rns _) =
    [ lid | (RuleNode lid _ _) <- lns ] `union` [ rid | (RuleNode rid _ _) <- rns ]


---------------------------------------------------------------------
-- Edge mangling
---------------------------------------------------------------------


-- There is no concept of an edge interface in GP2, so we need to be 
-- clever about matching similar edges on the LHS and RHS of the rule.
--
-- The situation is complicated by the presence of bidi rule edges

-- Wrapper function for edge-handling complexity!
makeIREdges :: AstRuleGraph -> AstRuleGraph -> OilrRule
makeIREdges (AstRuleGraph _ les) (AstRuleGraph _ res) =
    packEdges [] [ makeIREdge e | e <- les ]
                 [ makeIREdge e | e <- res ]


-- AstRuleEdge EdgeName Bool NodeName NodeName RuleLabel

-- Match LHS and RHS edges based on structural equality
packEdges :: OilrRule -> [OilrElem] -> [OilrElem] -> OilrRule
packEdges acc [] []     = reverse acc
packEdges acc [] (r:rs) =        packEdges (Create r:acc) [] rs
packEdges acc (l:ls) [] =        packEdges (Delete l:acc) ls []
packEdges acc (l:ls) rs = case partition (==l) rs of
    (r:_, _) ->                  packEdges (Same l            :acc) ls (delete r rs)
    ([] , _) -> case partition (irEdgeSimilarity l) rs of
                    (r:_, _)  -> packEdges (Change l r        :acc) ls (delete r rs)
                    ([],  _)  -> packEdges (Delete l:acc) ls rs
    -- TODO: make this more efficient by distinguishing structural changes from textual

-- Edge equality test (including labels)
edgeEquality :: AstRuleEdge -> AstRuleEdge -> Bool
edgeEquality (AstRuleEdge _ False lsrc ltgt ll) (AstRuleEdge _ False rsrc rtgt rl) = 
    lsrc == rsrc && ltgt == rtgt && ll == rl
edgeEquality (AstRuleEdge _ True _ _ _) (AstRuleEdge _ False _ _ _) = False
edgeEquality (AstRuleEdge _ True lsrc ltgt ll) (AstRuleEdge _ True rsrc rtgt rl) = 
    ( lsrc == rsrc && ltgt == rtgt || lsrc == rtgt && rsrc == ltgt ) && ll == rl


-- Test for GP2-valid structural equality between edges (not considering labels)
irEdgeSimilarity :: OilrElem -> OilrElem -> Bool
irEdgeSimilarity (IREdge _ _ _ True  lsrc ltgt) (IREdge _ _ _ bi rsrc rtgt)
    | (lsrc == rsrc && ltgt == rtgt ) = bi
    | (lsrc == rtgt && ltgt == rsrc ) = bi
    | otherwise                       = False
irEdgeSimilarity (IREdge _ _ _ False lsrc ltgt) (IREdge _ _ _ False rsrc rtgt)
    | lsrc == rsrc && ltgt == rtgt  = True
    | otherwise                     = False
-- TODO: does this consider manipulation of variables!?!



-- TODO: edge id?
makeIREdge :: AstRuleEdge -> OilrElem
makeIREdge (AstRuleEdge _ bidi src tgt (RuleLabel l c)) =
    IREdge "" c (makeIRLabel l) bidi src tgt


---------------------------------------------------------------------
-- IR creation functions
---------------------------------------------------------------------

irNode :: Mapping Id Sig -> (Maybe RuleNode, Maybe RuleNode) -> OilrMod
irNode sigs = irElem (makeIRNode sigs) (==)

irEdge :: (Maybe AstRuleEdge, Maybe AstRuleEdge) -> OilrMod
irEdge = irElem makeIREdge edgeEquality

irElem :: (a -> OilrElem) -> (a -> a -> Bool) -> (Maybe a, Maybe a) -> OilrMod
irElem mkElem _   (Just l, Nothing) = Delete (mkElem l)
irElem mkElem _   (Nothing, Just r) = Create (mkElem r)
irElem mkElem eql (Just l,  Just r)
            | l `eql` r  =  Same   (mkElem l)
            | otherwise  =  Change (mkElem l) (mkElem r)

makeIRNode :: Mapping Id Sig -> RuleNode -> OilrElem
makeIRNode sigs (RuleNode id root (RuleLabel l c)) = IRNode id c i sig
    where i = makeIRLabel l
          sig = case lookup id sigs of
                    Nothing -> NoSig
                    Just s  -> s

makeIRLabel :: [RuleAtom] -> IRLabel
makeIRLabel []                  = IREmpty
makeIRLabel [Val (Int i)]       = IRInt i
makeIRLabel [Val v]             = error $ "Unsupported literal value: " ++ show v
makeIRLabel [Var (v, IntVar)]   = IRVar v
makeIRLabel [Var (v, ListVar)]  = IRLst v  -- TODO: only valid if v is not evaluated!
makeIRLabel [Var (v, t)]        = error $ v ++ " is of unsupported type: " ++ show t
makeIRLabel [atom]              = error $ "Unsupported atom: " ++ show atom
makeIRLabel (x:xs)              = error "List type labels are not supported"


