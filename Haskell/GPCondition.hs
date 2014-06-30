module GPCondition where

import ParseLib
import ParseRule
import ParseProgram
import ParseGraph
import GPSyntax
import ProcessAst
import LabelMatch
import GraphMatch
import Graph
import ExAr
import Data.List
import Data.Maybe


testastrule :: AstRule
testastrule = (AstRule "rule1" 
               [("i",IntVar),("l",ListVar)] 
              (AstRuleGraph [RuleNode "n1" False (RuleLabel [Var ("i",ListVar)] Uncoloured),
                             RuleNode "n2" False (RuleLabel [Var ("l",ListVar)] Uncoloured)]
                            [RuleEdge False "n1" "n2" (RuleLabel [] Uncoloured)],
               AstRuleGraph [RuleNode "n1" False (RuleLabel [Var ("i",ListVar)] Uncoloured),
                             RuleNode "n2" False (RuleLabel [Var ("l",ListVar),Var ("i",ListVar)] Uncoloured)] 
                            [RuleEdge False "n1" "n2" (RuleLabel [] Uncoloured)])
              ["n1","n2"] 
              (Edge "n1" "n2" Nothing) 
              "true")

testLHS :: AstRuleGraph
testLHS = (AstRuleGraph [RuleNode "n1" False (RuleLabel [Var ("i",ListVar)] Uncoloured),
                         RuleNode "n2" False (RuleLabel [Var ("l",ListVar)] Uncoloured)]
                        [RuleEdge False "n1" "n2" (RuleLabel [] Uncoloured)])

testhg :: AstHostGraph
testhg = AstHostGraph 
        [HostNode "n1" False (HostLabel [Int 1] Uncoloured),
         HostNode "n2" False (HostLabel [Str "hello"] Uncoloured),
         HostNode "n3" False (HostLabel [Int 1, Int 2] Uncoloured)]
        [HostEdge "n1" "n2" (HostLabel [] Uncoloured),
         HostEdge "n1" "n2" (HostLabel [] Uncoloured)]

testtab :: SymbolTable
testtab = makeTable slist

slist :: SymbolList
slist = [("i", Symbol (Var_S IntVar False) "Global" "r1"),
         ("l", Symbol (Var_S ListVar False) "Global" "r1")]

getNodeId :: HostGraph -> NodeName -> NodeId
getNodeId g id = case candidates of
        [] -> error $ "ID " ++ id ++ " not found"
        [nid] -> nid
        _  -> error $ "Duplicate ID found! Eep!"
    where
        candidates = filter (matchID . fromJust . nLabel g) $ allNodes g
        matchID :: HostNode -> Bool
        matchID (HostNode i _ _) = i == id


getNodeName :: HostGraph -> NodeId -> NodeName
getNodeName g nid = case nLabel g nid of
        Nothing -> error "Fail!"
        Just ( HostNode id _ _ ) -> id

-- type Environment = Subst ID [HostAtom]

-- Given a graph morphism (containing a variable-value mapping) and a host graph,
-- a rule label is transformed into a host label (list of constants) by evaluating
-- any operators (degree, length) and substituting variables for their values
-- according to the morphism.
labelEval :: GraphMorphism -> HostGraph -> RuleLabel -> HostLabel
labelEval m g (RuleLabel list col) = HostLabel (concatMap (atomEval m g) list) col

atomEval :: GraphMorphism -> HostGraph -> RuleAtom -> [HostAtom]
atomEval m@(GM env _ _) g a = case a of
   -- TODO: error check
   Var (name, gpType) -> fromJust $ lookup name env 
   Val ha -> [ha]
   -- Degree operators assume node exists in the morphism/LHS graph.
   Indeg node -> [Int $ intExpEval m g a]
   Outdeg node -> [Int $ intExpEval m g a]
   Llength list -> [Int $ length list]
   Slength exp -> [Int $ length $ stringExpEval env exp]
   Neg exp -> [Int $ intExpEval m g a]
   Plus exp1 exp2 -> [Int $ intExpEval m g a]
   Minus exp1 exp2 -> [Int $ intExpEval m g a]
   Times exp1 exp2 -> [Int $ intExpEval m g a]
   Div exp1 exp2 ->  [Int $ intExpEval m g a]
   exp@(Concat exp1 exp2) -> [Str $ stringExpEval env exp]

-- Expects a RuleAtom representing an integer expression. 
intExpEval :: GraphMorphism -> HostGraph -> RuleAtom -> Int
intExpEval m@(GM env nms _) g a = case a of
   Var (name, IntVar) -> let Just [Int i] = lookup name env in i
   Val (Int i) -> i
   Indeg node -> let Just hnode = lookup (getNodeId g node) nms 
                 in length $ inEdges g hnode
   Outdeg node -> let Just hnode = lookup (getNodeId g node) nms 
                  in length $ outEdges g hnode
   Llength list -> length list
   Slength exp -> length $ stringExpEval env exp
   Neg exp -> 0 - intExpEval m g exp
   Plus exp1 exp2 -> intExpEval m g exp1 + intExpEval m g exp2
   Minus exp1 exp2 -> intExpEval m g exp1 - intExpEval m g exp2
   Times exp1 exp2 -> intExpEval m g exp1 * intExpEval m g exp2
   -- TODO: handle division by 0
   Div exp1 exp2 -> intExpEval m g exp1 `div` intExpEval m g exp2
   _ -> error "Not an integer expression."

-- Expects a RuleAtom representing a string expression. 
stringExpEval :: Environment -> RuleAtom -> String
stringExpEval env a = case a of
   Var (name, ChrVar) -> let Just [Chr c] = lookup name env in "c"
   Var (name, StrVar) -> let Just [Str s] = lookup name env in s
   Val (Chr c) -> "c"
   Val (Str s) -> s 
   Concat exp1 exp2 -> stringExpEval env exp1 ++ stringExpEval env exp2
   _ -> error "Not a string expression."


conditionEval :: Condition -> GraphMorphism -> HostGraph -> Bool
conditionEval c m@(GM env nms _) g = 
  case c of
     NoCondition -> True
     TestInt name -> 
        let var = lookup name env
        in
           case var of
               Nothing       -> False
               Just ([Int _]) -> True
               _             -> False

     TestChr name -> 
        let var = lookup name env
        in
           case var of
               Nothing         -> False
               Just ([Chr _]) -> True
               _               -> False                         

     TestStr name -> 
        let var = lookup name env
        in
           case var of
               Nothing        -> False
               Just ([Str _]) -> True
               _              -> False

     TestAtom name -> 
        let var = lookup name env
        in
           case var of
               Nothing      -> False
               Just ([_])   -> True
               _            -> False

     Edge src tgt maybeLabel -> 
        let label = fromMaybe (RuleLabel [] Uncoloured) maybeLabel
            hsrc = lookup (getNodeId g src) nms
            htgt = lookup (getNodeId g tgt) nms
            hlabel = labelEval m g label
        in
           if (isNothing hsrc || isNothing htgt) 
              then False
              else foldr (labelCompare hlabel) False (joiningEdges g (fromJust hsrc) (fromJust htgt))
        where 
        -- Should be RuleLabel. Use eval functions.
        -- labelCompare :: (HostLabel -> EdgeId -> Bool -> Bool)
           labelCompare _ _ True = True
           labelCompare hlabel e False = 
              case (eLabel g e) of
                 Nothing     -> False
                 Just label  -> label == hlabel

     Eq l1 l2 -> and $ zipWith (==) (concatMap (atomEval m g) l1) (concatMap (atomEval m g) l2)

     NEq l1 l2 -> or $ zipWith (/=) (concatMap (atomEval m g) l1) (concatMap (atomEval m g) l2)

     -- GP2 semantics requires atoms in relational conditions to be integer 
     -- expressions. 

     Greater a1 a2 -> intExpEval m g a1 > intExpEval m g a2
 
     GreaterEq a1 a2 -> intExpEval m g a1 >= intExpEval m g a2
 
     Less a1 a2 -> intExpEval m g a1 < intExpEval m g a2

     LessEq a1 a2 -> intExpEval m g a1 <= intExpEval m g a2

     Not cond -> not $ conditionEval cond m g 
 
     Or cond1 cond2 -> (conditionEval cond1 m g) || (conditionEval cond2 m g)

     And cond1 cond2 -> (conditionEval cond1 m g) && (conditionEval cond2 m g)




