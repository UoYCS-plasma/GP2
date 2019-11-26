module LabelMatch where

import Prelude hiding (lookup)
import Data.List
import Data.Maybe
import Control.Monad (guard)
import Mapping
import GPSyntax

type Environment = Mapping VarName [HostAtom]

doLabelsMatch :: HostLabel -> RuleLabel -> Maybe Environment
doLabelsMatch (HostLabel has hc) (RuleLabel ras rc) = if colourMatch hc rc then atomsMatch has ras else Nothing

colourMatch :: Colour -> Colour -> Bool
colourMatch _  Any = True
colourMatch hc rc   = (hc == rc)

atomsMatch :: [HostAtom] -> [RuleAtom] -> Maybe Environment
atomsMatch = atomsMatchWith []

-- The core of the label matcher. There are a number of base cases:
-- 
-- (1) The end of both lists is reached at the same time. Return the 
--     current environment.
-- (2) The end of the rule list has been reached but there are still unchecked
--     items in the host list. The labels do not match.
-- (3) The end of the host list has been reached but there are still unchecked
--     items in the rule list. If the rule list contains only a list variable,
--     then assign it the empty list. Nothing else can match the empty list,
--     so any other remaining rule list means the labels do not match.
-- 
-- The function compares atoms one at a time. Most of it is straightforward:
-- constants are checked for equality and variable-value mappings are added
-- to the environment provided they are of the correct type.
-- 
-- GP2 semantics allow only one list variable in a list. Hence we use the 
-- lengths of both lists to assign the list variable the list of appropriate 
-- length, then recursively call atomsMatchWith on the remaining host list.
-- 
-- Concat expressions are handled by calling the auxiliary function 'expand'
-- to transform the expression into a list L of RuleAtoms. L is then compared
-- with the current host atom with the stringsMatchWith function which operates
-- analogously to atomsMatchWith,
atomsMatchWith :: Environment -> [HostAtom] -> [RuleAtom] -> Maybe Environment
atomsMatchWith env [] [] = Just env
atomsMatchWith env [] [Var (var, ListVar)] = extendMapping env var []
atomsMatchWith env hall@(ha:has) (ra:ras) =
    case (ha, ra) of
        ( _    , Var (var, ListVar) ) ->
            let hl = length hall ; rl = length ras ; d  = hl - rl 
            in  case compare hl rl of
                LT -> Nothing
                EQ -> do env' <- extendMapping env var []
                         atomsMatchWith env' hall ras
                GT -> do env' <- extendMapping env var (take d hall)
                         atomsMatchWith env' (drop d hall) ras              
        ( Int i, Val (Int j) ) -> do
            guard $ i == j
            atomsMatchWith env has ras
        ( Int i, Var (var, vt) ) -> do
            guard $ IntVar <= vt
            env' <- extendMapping env var [ha]
            atomsMatchWith env' has ras
        ( Chr c, Val (Chr d) ) -> do
            guard $ c == d
            atomsMatchWith env has ras
        ( Chr c, Var (var, vt) ) -> do
            guard $ ChrVar <= vt
            env' <- extendMapping env var [ha]
            atomsMatchWith env' has ras
        ( Str str, Val (Chr c) ) -> do
            guard $ str /= "" && head str == c
            atomsMatchWith env has ras
        ( Str str, Val (Str s) ) -> do
            guard $ str == s
            atomsMatchWith env has ras
        ( Str str, Var (var, ChrVar) ) -> do
            guard $ length str == 1
            env' <- extendMapping env var [(Chr $ head str)]
            atomsMatchWith env' has ras
        ( Str str, Var (var, vt) ) -> do
            guard $ StrVar <= vt
            env' <- extendMapping env var [ha]
            atomsMatchWith env' has ras
        ( Str str, a@(Concat a1 a2) ) -> do
            env' <- stringMatchWith env str (expand a)
            atomsMatchWith env' has ras
        _ -> Nothing
atomsMatchWith env _ _ = Nothing

expand :: RuleAtom -> [RuleAtom]
expand (Concat a1 a2) = expand a1 ++ expand a2
expand a = [a]

-- Matches a host string with a list of rule atoms. Each rule atom is
-- a string expression: a character constant/variable or a string constant/
-- variable. The function operates almost identically to atomsMatchWith.

stringMatchWith :: Environment -> String -> [RuleAtom] -> Maybe Environment
stringMatchWith env [] [] = Just env
stringMatchWith env [] [Var (var, StrVar)] = extendMapping env var [Str ""] 
stringMatchWith env str@(c:cs) (a:as) = 
   case a of
        Val (Chr d) -> do
           guard $ c == d
           stringMatchWith env cs as
        Val (Str s) -> do
           guard $ s `isPrefixOf` str 
           stringMatchWith env (drop (length s) str) as
        Var (var, ChrVar) -> do
           guard $ str /= ""
           env' <- extendMapping env var [(Chr c)]
           stringMatchWith env' cs as
        Var (var, StrVar) -> 
           let sl = length str ; al = length as ; d = sl - al
           in case compare sl al of
              LT -> Nothing
              EQ -> do env' <- extendMapping env var []
                       stringMatchWith env' str as
              GT -> do env' <- extendMapping env var [(Str $ take d str)]
                       stringMatchWith env' (drop d str) as
        _ -> Nothing
stringMatchWith env _ _ = Nothing
