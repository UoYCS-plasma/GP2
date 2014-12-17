module List where

import Data.List (permutations)
import GHC.Exts (groupWith)
import Control.Monad (guard)

-- Given a list of lists, compute all ways of choosing a single item
-- from each inner list to obtain a list of choices.
-- Eg. choices [[1,2],[3,4,5]] = [[1,3],[1,4],[1,5],[2,3],[2,4],[2,5]]
choices :: [[a]] -> [[a]]
choices []       = [[]]
choices (xs:xss) = [c:cs | c <- xs, cs <- choices xss]

sublistsOf :: Int -> [a] -> [[a]]
sublistsOf 0 _        = [[]]
sublistsOf _ []       = []
sublistsOf n (x:xs)   = map (x:) (sublistsOf (n-1) xs) ++ sublistsOf n xs

isSet :: Eq a => [a] -> Bool
isSet []      =  True
isSet (x:xs)  =  x `notElem` xs && isSet xs

representBy :: (a->a->Bool) -> [a] -> [(a,Int)]
representBy equiv xs  =  foldl add [] xs
  where
  add []             y  =  [(y,1)]
  add (xn@(x,n):xns) y  =  if x `equiv` y then (x,n+1):xns else (xn : add xns y)

nonEmpty :: [a] -> Bool
nonEmpty []     =  False
nonEmpty (_:_)  =  True

bijectionsWith :: Ord c => (a->c) -> [a] -> (b->c) -> [b] -> [[(a,b)]]
bijectionsWith f xs g ys =
  case blockZip (groupWith f xs) (groupWith g ys) of
  Nothing  -> []
  Just zbs -> [ concat bp
              | bp <- choices [ [zip b1 b2' | b2' <- permutations b2]
                              | (b1,b2) <- zbs ] ] 

blockZip :: [[a]] -> [[b]] -> Maybe [([a],[b])]
blockZip [] [] = Just []
blockZip (xs:xss) (ys:yss) = do
  guard (length xs == length ys)
  xyss <- blockZip xss yss
  return ((xs,ys) : xyss) 

