module GPCondition where

import GPSyntax
import LabelMatch
import GraphMatch
import Graph
import Data.List
import Data.Maybe

-- type Environment = Subst ID [HostAtom]

-- Assumed functions.
labelEval :: Environment -> RuleLabel -> HostLabel
labelEval _ _ = HostLabel [] Uncoloured

atomEval :: Environment -> RuleAtom -> HostAtom
atomEval _ _ = Int 1 

intEval :: Environment -> RuleAtom -> Int
intEval _ _ = 1
-- intEval nonIntExp = error "not an integer expression"

conditionEval :: Condition -> GraphMorphism -> HostGraph -> Environment -> Bool
conditionEval c m g env = 
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
     -- src and tgt are string node identifiers from the text file, while
     -- the morphism and host graph refer to the integer node IDs of the
     -- graph data structure. Hmm. This should probably be solved in the
     -- phase that transforms the AST using the NodeMap from processAst.
     -- I assume appropriate node IDs for now.
     Edge src tgt maybeLabel -> 
        let label = fromMaybe (RuleLabel [] Uncoloured) maybeLabel
            hsrc = lookup src m
            htgt = lookup tgt m
            hlabel = labelEval env label
        in
           if (isNothing hsrc || isNothing htgt) 
              then False
              else foldr (labelCompare hlabel) False (joiningEdges g hsrc htgt)
        where 
        -- labelCompare :: (HostLabel -> EdgeId -> Bool -> Bool)
           labelCompare _ _ True = True
           labelCompare hlabel e False = 
              case (eLabel g e) of
                 Nothing     -> False
                 Just label  -> label == hlabel

     Eq l1 l2 -> and $ zipWith (==) (map (atomEval env) l1) (map (atomEval env) l2)

     NEq l1 l2 -> or $ zipWith (/=) (map (atomEval env) l1) (map (atomEval env) l2)

     -- GP2 semantics requires atoms in relational conditions to be integer 
     -- expressions. 

     Greater a1 a2 -> intEval env a1 > intEval env a2
 
     GreaterEq a1 a2 -> intEval env a1 >= intEval env a2
 
     Less a1 a2 -> intEval env a1 < intEval env a2

     LessEq a1 a2 -> intEval env a1 <= intEval env a2

     Not cond -> not $ conditionEval cond m g env
 
     Or cond1 cond2 -> (conditionEval cond1 m g env) || (conditionEval cond2 m g env)

     And cond1 cond2 -> (conditionEval cond1 m g env) && (conditionEval cond2 m g env)




