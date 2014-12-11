module List where

import Data.List (permutations)

choices :: [[a]] -> [[a]]
choices []       = [[]]
choices (xs:xss) = [c:cs | c <- xs, cs <- choices xss]

permutedSizedSubsets :: Int -> [a] -> [[a]]
permutedSizedSubsets k xs = concatMap permutations $ sublistsOf k xs

sublistsOf :: Int -> [a] -> [[a]]
sublistsOf 0 _        = [[]]
sublistsOf _ []       = []
sublistsOf n (x:xs)   = map (x:) (sublistsOf (n-1) xs) ++ sublistsOf n xs
