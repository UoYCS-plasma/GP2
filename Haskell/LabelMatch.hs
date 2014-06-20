module LabelMatch where

import Prelude hiding (lookup)
import Data.List
import Data.Maybe
import Control.Monad (guard)

import ExAr
import GPGraph
import Graph
import GPSyntax

type Subst a b = [(a, b)]


substMerge :: ( Eq a, Eq b ) => Subst a b -> Subst a b -> Maybe (Subst a b)
substMerge s [] = Just s
substMerge s ((k, v):kvs) = do
    s' <- substExtend s k v
    substMerge s' kvs


substExtend :: ( Eq a, Eq b ) => Subst a b -> a -> b -> Maybe (Subst a b)
--substExtend :: Environment -> ID -> [HostAtom] -> Maybe Environment
substExtend s key val = 
    case lookup key s of
            Nothing   -> Just $ (key, val):s
            Just v -> if v == val then Just s else Nothing


atomsMatch :: [HostAtom] -> [RuleAtom] -> Maybe Environment
atomsMatch = atomsMatchWith []

atomsMatchWith :: Environment -> [HostAtom] -> [RuleAtom] -> Maybe Environment
atomsMatchWith env [] [] = Just env
atomsMatchWith env hall@(ha:has) (ra:ras) =
    case (ha, ra) of
        ( _    , Var (var, ListVar) ) ->
            case compare hl rl of
                LT -> Nothing
                EQ -> do 
                    env' <- substExtend env var []
                    atomsMatchWith env' hall ras
                GT -> do
                    env' <- substExtend env var $ take n hall
                    atomsMatchWith env' (drop n hall) ras
            where
                hl = length hall
                rl = length ras
                n  = hl - rl                
        ( Int i, Val (Int j) ) -> do
            guard $ i == j
            atomsMatchWith env has ras
        ( Int i, Var (var, vt) ) -> do
            guard $ IntVar <= vt
            env' <- substExtend env var [ha]
            atomsMatchWith env' has ras
        ( Chr c, Val (Chr d) ) -> do
            guard $ c == d
            atomsMatchWith env has ras
        ( Chr c, Var (var, vt) ) -> do
            guard $ ChrVar <= vt
            env' <- substExtend env var [ha]
            atomsMatchWith env' has ras
        ( Str str, Val (Chr c) ) -> do
            guard $ str != ""
            guard $ head str == c
            atomsMatchWith env has ras
        ( Str str, Val (Str s) ) -> do
            guard $ str == s
            atomsMatchWith env has ras
        ( Str str, Var (var, vt) ) -> do
            guard $ StrVar <= vt
            env' <- substExtend env var [ha]
            atomsMatchWith env' has ras
-- Don't know how to handle this case yet. Perhaps the intermediate phase
-- can transform a Concat expression into a list of "string atoms" which 
-- can then be processed similarly to atomsMatchWith's handling of lists of
-- atoms.
--     ( Str str, Concat a1 a2 ) -> 

{- Returns a Maybe pair of the updated environment and the "unparsed" portion of 
-- the host string.
stringMatchWith :: Environment -> String -> RuleAtom -> Maybe (Environment, String)
stringMatchWith env str a = 
   case a of
        Chr c -> do
           guard $ s != ""
           guard $ head str == s
           return (env, tail s)
        Str s -> do
           guard $ s `isPrefixOf` str 
           let rl = length s
           return (env, drop rl str)
        Var (var, ChrVar) -> do
           guard $ str != ""
           env' <- substExtend env var $ head str
           return (env', tail str)
        Var (var, StrVar) -> do
           
        Concat a1 a2 -> do
           env' <- stringMatchWith env str a1 
           stringMatchWith env' str a2
-}
        

colourMatch :: Colour -> Colour -> Bool
colourMatch _  Cyan = True
colourMatch hc rc   = (hc == rc)

doLabelsMatch :: HostLabel -> RuleLabel -> Maybe Environment
doLabelsMatch (HostLabel has hc) (RuleLabel ras rc) = if colourMatch hc rc then atomsMatch has ras else Nothing

