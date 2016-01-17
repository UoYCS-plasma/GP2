module OILR3.Optimiser where

import Mapping
import OILR3.IR
import Debug.Trace

{-
data OilrExpr = IRSeqn [OilrExpr]
              | IRIf   OilrExpr OilrExpr OilrExpr
              | IRTry  OilrExpr OilrExpr OilrExpr
              | IRTrns OilrExpr   -- transaction that rolls-back if OilrExpr fails
              | IRRuleSet [Id] | IRCall Id | IRLoop OilrExpr
              | IRTrue | IRFals
     deriving (Show, Eq)
 -}

optimise :: [OilrIR] -> [OilrIR]
optimise is = map optimiseIR $ inliner is

optimiseIR :: OilrIR -> OilrIR
-- Loops at the top-level do not need backtracking, because as soon as an 
-- element of the sequence fails the whole program fails.
optimiseIR (IRProc "Main" (IRTrns e@(IRSeqn _))) = IRProc "Main" $ optimiseExpr e
optimiseIR (IRProc id e)                         = IRProc id     $ optimiseExpr e
optimiseIR (IRRule id r)                         = IRRule id     $ optimiseRule r

optimiseExpr :: OilrExpr -> OilrExpr
-- Move transaction for sequences that begin with predicate rules...
optimiseExpr (IRTrns (IRSeqn [e])) = optimiseExpr e
optimiseExpr (IRTrns (IRSeqn (e:es)))
    | all isLoop es = IRSeqn (optimiseExpr e:[optimiseExpr e' | e' <- es])
    | isPredicate e = IRSeqn [ optimiseExpr e,  optimiseExpr (IRTrns (IRSeqn es)) ]
    | otherwise  = IRTrns $ IRSeqn $ map optimiseExpr (e:es)
-- Simplify singlet sequences
optimiseExpr (IRSeqn [x]) = optimiseExpr x
-- Remove unneeded transactions introduced by previous rule
optimiseExpr (IRTrns e) = case e of
    (IRSeqn [e]) -> optimiseExpr e
    (IRSeqn es)  -> IRTrns $ IRSeqn $ map optimiseExpr es
    _            -> optimiseExpr e
-- Terminal cases
optimiseExpr (IRLoop e) = IRLoop $ optimiseExpr e
optimiseExpr (IRIf (IRRuleSet rs) th el)
    -- TODO: Optimise more non-transactional if cases...
    | all isPred rs = IRTry (IRRuleSet rs) th el
optimiseExpr e = e


-- How the inliner works...
--
-- Count calls to each proc (and perhaps later rule...?)
-- While there is still an inlinable function in prog:
--    replace call with proc body

inliner :: [OilrIR] -> [OilrIR]
inliner prog = reap (map fst inlinables) $ inline inlinables $ trace (show inlinables) prog
    where 
          inlinables = [ (id, body) | ((id, n), body) <- zip callCounts es, n==1 ]
          callCounts = foldr countCalls cs es
          (cs, es) = unzip [ ((id, 0), body)
                             | def <- prog
                             , let (id, body) = decompose def, id /= "" ]
          decompose :: OilrIR -> (Id, OilrExpr)
          decompose (IRProc id e) = (id, e)
          decompose (IRRule id _) = ("", IRFals) -- this inliner doesn't care about Rules

reap :: [Id] -> [OilrIR] -> [OilrIR]
reap inls (IRProc id _:ds) | id `elem` inls = reap inls ds
reap inls (d:ds) = d:reap inls ds
reap inls [] = []

inline :: Mapping Id OilrExpr -> [OilrIR] -> [OilrIR]
inline inls (IRProc id e : defs) = ( IRProc id $ replaceCalls inls e ) : inline inls defs
inline _ x = x

countCalls :: OilrExpr -> [(Id, Int)] -> [(Id, Int)]
countCalls (IRSeqn es) cs = foldr countCalls cs es
countCalls (IRTrns e) cs = countCalls e cs
countCalls (IRLoop e) cs = countCalls e cs
countCalls (IRCall id) cs = [ (p, n') | (p, n) <- cs , let n' = if id == p then n+1 else n ]
-- countCalls (IRRuleSet rs) cs = [ (r, n') | (r,n) <- cs, let n' = n + length (filter (==r) rs)]
countCalls (IRTry c t e) cs = foldr countCalls cs [c,t,e]
countCalls (IRIf c t e) cs = foldr countCalls cs [c,t,e]
countCalls _ cs = cs

replaceCalls :: Mapping Id OilrExpr -> OilrExpr -> OilrExpr
replaceCalls inls (IRCall p) = case lookup p inls of
                                   Nothing -> IRCall p
                                   Just body -> replaceCalls inls body
replaceCalls inls (IRSeqn es) = IRSeqn $ map (replaceCalls inls) es
replaceCalls inls (IRTrns e)  = IRTrns $ replaceCalls inls e
replaceCalls inls (IRLoop e)  = IRLoop $ replaceCalls inls e
replaceCalls inls (IRTry c t e) = IRTry (replaceCalls inls c) (replaceCalls inls t) (replaceCalls inls e)
replaceCalls inls (IRIf c t e) = IRIf (replaceCalls inls c) (replaceCalls inls t) (replaceCalls inls e)
replaceCalls inls e           = e



-- Identify inliner-candidates
occurrences :: String -> [OilrIR] -> Int
occurrences id defs = count $ map (\(IRProc _ e) -> e) defs
    where count = sum . (map findCalls)
          findCalls (IRSeqn es) = count es
          findCalls (IRIf c t e) = count [c,t,e]
          findCalls (IRTry c t e) = count [c,t,e]
          findCalls (IRTrns e) = findCalls e
          findCalls (IRCall p) | p==id = 1
                               | otherwise = 0
          findCalls _ = 0

-- 0 -> never inline
-- 1 -> inline procs and rules that are only called once
-- n -> inline procs and rules called at most n times
inlineThres = 1

isProc :: OilrIR -> Bool
isProc (IRProc _ _) = True
isProc _            = False

isLoop :: OilrExpr -> Bool
isLoop (IRLoop _) = True
isLoop _          = False

-- todo: implementation
isPred _ = False

isPredicate :: OilrExpr -> Bool
isPredicate _ = False

optimiseRule :: OilrRule -> OilrRule
optimiseRule r = r

