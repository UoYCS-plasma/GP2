module GPCondition where

import GPSyntax
import LabelMatch
import GraphMatch
import Graph
import Data.List
import Data.Maybe

-- type Environment = Subst ID [HostAtom]

-- Assumed functions.
labelEval :: GraphMorphism -> HostGraph -> RuleLabel -> HostLabel
labelEval m g (RuleLabel list col) = HostLabel (concatMap (atomEval m g) list) col

atomEval :: GraphMorphism -> HostGraph -> RuleAtom -> [HostAtom]
atomEval m@(GM env nms _) g a = case a of
   -- TODO: error check
   Var (name, gpType) -> fromJust $ lookup name env 
   Val ha -> [ha]
   -- String node IDs. Require integers.
   Indeg node -> let hnode = lookup node nms in
                  length $ inEdges g hnode
   Outdeg node -> let hnode = lookup node nms in
                  length $ outEdges g hnode
   Llength list -> length list 
   Slength exp -> case exp of
                      Var (Var (name, ChrVar)) -> [1]
                      Var (Var (name, StrVar)) -> [length $ fromJust $ lookup name env]
                      -- This is wrong: the Concat expression may contain a string variable.
                      str@(Concat s t) -> [length $ expand str]
                      -- Slength not applicable to non-string expressions. 
                      -- Probably should return an error.
                      _ -> [0]
   Neg exp -> [0 - intExpEval env exp]
   Plus exp1 exp2 -> [intExpEval env exp1 + intExpEval env exp2]
   Minus exp1 exp2 -> [intExpEval env exp1 - intExpEval env exp2]
   Times exp1 exp2 -> [intExpEval env exp1 * intExpEval env exp2]
   -- TODO: handle division by 0
   Div exp1 exp2 -> [intExpEval env exp1 `div` intExpEval env exp2]
   exp@(Concat exp1 exp2) -> map atomEval $ expand exp

intExpEval :: Environment -> RuleAtom -> Int
intExpEval _ _ = 1
--intExpEval nonIntExp = error "not an integer expression"

conditionEval :: Condition NodeId -> GraphMorphism -> HostGraph -> Bool
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
            hsrc = lookup src nms
            htgt = lookup tgt nms
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

     Greater a1 a2 -> intExpEval env a1 > intExpEval env a2
 
     GreaterEq a1 a2 -> intExpEval env a1 >= intExpEval env a2
 
     Less a1 a2 -> intExpEval env a1 < intExpEval env a2

     LessEq a1 a2 -> intExpEval env a1 <= intExpEval env a2

     Not cond -> not $ conditionEval cond m g 
 
     Or cond1 cond2 -> (conditionEval cond1 m g) || (conditionEval cond2 m g)

     And cond1 cond2 -> (conditionEval cond1 m g) && (conditionEval cond2 m g)




