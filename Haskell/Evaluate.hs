module Evaluate where

import Data.List
import Data.Maybe
import GPSyntax
import ProcessAst
import Mapping
import LabelMatch
import GraphMatch
import Graph

type EvalContext = (GraphMorphism, HostGraph, RuleGraph)

-- LABEL EVALUATION
-- Given a graph morphism (containing a variable-value mapping) and a host graph,
-- a rule label is transformed into a host label (list of constants) by evaluating
-- any operators (degree, length,...) and substituting variables for their values
-- according to the morphism.

labelEval :: EvalContext -> RuleLabel -> HostLabel
labelEval ec (RuleLabel list col) = HostLabel (atomListEval ec list) col

atomEval :: EvalContext -> RuleAtom -> [HostAtom]
atomEval ec@(GM env _ _, _, _) a = case a of
   Val ha                 -> [ha]
   Var (name,_)           -> definiteLookup name env 
   Llength list           -> [Int $ length list]
   Slength exp            -> [Int $ length $ stringExpEval env exp]
   exp@(Concat exp1 exp2) -> [Str $ stringExpEval env exp]
   _                      -> [Int $ intExpEval ec a]
 
atomListEval :: EvalContext -> [RuleAtom] -> [HostAtom]
atomListEval ec ras = [ha | ra <- ras, ha <- atomEval ec ra]

-- Expects a RuleAtom representing an integer expression. 
intExpEval :: EvalContext -> RuleAtom -> Int
intExpEval ec@(GM env _ _, _, _) a = case a of
   Val (Int i)        -> i
   Var (name, IntVar) -> let [Int i] = definiteLookup name env in i
   Indeg node         -> degExpEval ec node inEdges
   Outdeg node        -> degExpEval ec node outEdges
   Llength list       -> length list
   Slength exp        -> length $ stringExpEval env exp
   Neg exp            -> 0 - intExpEval ec exp
   Plus exp1 exp2     -> intExpEval ec exp1 + intExpEval ec exp2
   Minus exp1 exp2    -> intExpEval ec exp1 - intExpEval ec exp2
   Times exp1 exp2    -> intExpEval ec exp1 * intExpEval ec exp2
   -- TODO: handle division by 0
   Div exp1 exp2      -> intExpEval ec exp1 `div` intExpEval ec exp2
   _                  -> error "Expecting an integer expression."

degExpEval :: EvalContext -> NodeName -> (HostGraph -> NodeId -> [EdgeId]) -> Int
degExpEval (GM _ nms _, g, r) node edges =
   case lookup (getNodeId r node) nms of
   Nothing     -> error "Argument of degree function not in the LHS."
   (Just node) -> length $ edges g node

-- Expects a RuleAtom representing a string expression. 
stringExpEval :: Environment -> RuleAtom -> String
stringExpEval env a = case a of
   Var (name, ChrVar) -> let [Chr c] = definiteLookup name env in "c"
   Var (name, StrVar) -> let [Str s] = definiteLookup name env in s
   Val (Chr c)        -> "c"
   Val (Str s)        -> s 
   Concat exp1 exp2   -> stringExpEval env exp1 ++ stringExpEval env exp2
   _                  -> error "Expecting a string expression."

-- CONDITION EVALUATION
conditionEval :: EvalContext -> Condition -> Bool
conditionEval ec@(GM env nms _, g, r) c = 
  case c of
  NoCondition   -> True
  TestInt name  -> isJust $ do [Int _] <- lookup name env ; return ()
  TestChr name  -> isJust $ do [Chr _] <- lookup name env ; return () 
  TestStr name  -> isJust $ do [Str _] <- lookup name env ; return ()
  TestAtom name -> isJust $ do [_]     <- lookup name env ; return ()
  Edge src tgt Nothing      -> not $ null $ edgesInHostGraph src tgt
  Edge src tgt (Just label) -> or [ eLabel g e == labelEval ec label
                                  | e <- edgesInHostGraph src tgt]
  Eq  l1 l2                 -> atomListEval ec l1 == atomListEval ec l2
  NEq l1 l2                 -> atomListEval ec l1 /= atomListEval ec l2
  -- Atoms in ordering conditions must be integer expressions. 
  Greater a1 a2             -> intExpEval ec a1 > intExpEval ec a2
  GreaterEq a1 a2           -> intExpEval ec a1 >= intExpEval ec a2
  Less a1 a2                -> intExpEval ec a1 < intExpEval ec a2
  LessEq a1 a2              -> intExpEval ec a1 <= intExpEval ec a2
  Not cond                  -> not $ conditionEval ec cond 
  Or cond1 cond2            -> conditionEval ec cond1 || conditionEval ec cond2 
  And cond1 cond2           -> conditionEval ec cond1 && conditionEval ec cond2 
  where
  edgesInHostGraph :: NodeName -> NodeName -> [HostEdgeId]
  edgesInHostGraph src tgt =
     case (lookup (getNodeId r src) nms, lookup (getNodeId r tgt) nms) of
     (Just s, Just t) -> joiningEdges g s t
     _ -> []

getNodeId :: RuleGraph -> NodeName -> NodeId
getNodeId r id = 
   case [n | n <- allNodes r, let RuleNode i _ _ = nLabel r n, i == id] of
   []    -> error $ "ID " ++ id ++ " not found"
   [nid] -> nid
   _     -> error $ "Duplicate ID found."

