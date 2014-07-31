module Mapping where

import Data.List
import Data.Maybe

-- A simple mapping from items of type a to items of type b.
type Mapping a b = [(a, b)]

mergeMapping :: ( Eq a, Eq b ) => Mapping a b -> Mapping a b -> Maybe (Mapping a b)
mergeMapping s [] = Just s
mergeMapping s ((k, v):kvs) = do
    s' <- extendMapping s k v
    mergeMapping s' kvs


extendMapping :: ( Eq a, Eq b ) => Mapping a b -> a -> b -> Maybe (Mapping a b)
extendMapping s key val = 
    case lookup key s of
            Nothing   -> Just $ (key, val):s
            Just v -> if v == val then Just s else Nothing

definiteLookup :: ( Eq a , Show a , Show b ) => a -> Mapping a b -> b
definiteLookup x xys = case lookup x xys of
        Just y -> y
        Nothing -> error $ "Element " ++ show x ++ " not found in mapping " ++ show xys

addItem :: Mapping a b -> a -> b -> Mapping a b
addItem items k v = ((k,v):items) 


