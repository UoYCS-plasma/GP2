module List where

import Data.List (permutations)

-- Given a list of lists, compute all ways of choosing a single item
-- from each inner list to obtain a list of choices.
-- Eg. choices [[1,2],[3,4,5]] = [[1,3],[1,4],[1,5],[2,3],[2,4],[2,5]]
choices :: [[a]] -> [[a]]
choices []       = [[]]
choices (xs:xss) = [c:cs | c <- xs, cs <- choices xss]

permutedSizedSubsets :: Int -> [a] -> [[a]]
permutedSizedSubsets k xs = concatMap permutations $ sublistsOf k xs

sublistsOf :: Int -> [a] -> [[a]]
sublistsOf 0 _        = [[]]
sublistsOf _ []       = []
sublistsOf n (x:xs)   = map (x:) (sublistsOf (n-1) xs) ++ sublistsOf n xs
