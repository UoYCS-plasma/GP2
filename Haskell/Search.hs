module Search where

import Prelude hiding (lookup)
import Data.List
import Data.Maybe
import Control.Monad (guard)

import ExAr
import GPGraph
import Graph
import GPSyntax

type Subst a b = [(a, b)]



notImplemented = error "Not implemented"

substMerge :: ( Eq a, Eq b ) => Subst a b -> Subst a b -> Maybe (Subst a b)
substMerge s [] = Just s
substMerge s ((k, v):kvs) = do
    s' <- substExtend s k v
    substMerge s' kvs


substExtend :: ( Eq a, Eq b ) => Subst a b -> a -> b -> Maybe (Subst a b)
--substExtend :: Subst ID [HostAtom] -> ID -> [HostAtom] -> Maybe ( Subst ID [HostAtom] )
substExtend s key val = 
    case lookup key s of
            Nothing   -> Just $ (key, val):s
            Just v -> if v == val then Just s else Nothing

-- indeg and outdeg not yet implemented
-- due to requirement for graph context and
-- ids of other nodes?
atomsMatch :: [HostAtom] -> [RuleAtom] -> Maybe (Subst ID [HostAtom])
atomsMatch = atomsMatchWith []

-- ListVar matching multiple elems is not yet 
-- implemented
atomsMatchWith :: Subst ID [HostAtom] -> [HostAtom] -> [RuleAtom] -> Maybe (Subst ID [HostAtom])
atomsMatchWith s [] [] = Just s
atomsMatchWith s hall@(ha:has) (ra:ras) =
    case (ha, ra) of
        ( _    , Var (var, ListVar) ) ->
            case compare hl rl of
                LT -> Nothing
                EQ -> do 
                    s' <- substExtend s var []
                    atomsMatchWith s' hall ras
                GT -> do
                    s' <- substExtend s var $ take n hall
                    atomsMatchWith s' (drop n hall) ras
            where
                hl = length hall
                rl = length ras
                n  = hl - rl                
        ( Int i, Val (Int j) ) -> do
            guard $ i == j
            atomsMatchWith s has ras
        ( Int i, Var (var, vt) ) -> do
            guard $ IntVar <= vt
            s' <- substExtend s var [ha]
            atomsMatchWith s' has ras
        ( Chr c, Val (Chr d) ) -> do
            guard $ c == d
            atomsMatchWith s has ras
        ( Chr c, Var (var, vt) ) -> do
            guard $ ChrVar <= vt
            s' <- substExtend s var [ha]
            atomsMatchWith s' has ras
        ( Str str, Val (Str t) ) -> do
            guard $ str == t
            atomsMatchWith s has ras
        ( Str str, Var (var, vt) ) -> do
            guard $ StrVar <= vt
            s' <- substExtend s var [ha]
            atomsMatchWith s' has ras

colourMatch :: Colour -> Colour -> Bool
colourMatch _  Cyan = True
colourMatch hc rc   = (hc == rc)

doLabelsMatch :: HostLabel -> RuleLabel -> Maybe (Subst ID [HostAtom])
doLabelsMatch (HostLabel has hc) (RuleLabel ras rc) = if colourMatch hc rc then atomsMatch has ras else Nothing

