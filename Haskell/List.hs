module List where

import Data.List (permutations,foldl')
import GHC.Exts (groupWith)
import Control.Monad (guard)

-- Given a list of lists, compute all ways of choosing a single item
-- from each inner list to obtain a list of choices.
-- Eg. choices [[1,2],[3,4,5]] = [[1,3],[1,4],[1,5],[2,3],[2,4],[2,5]]
choices :: [[a]] -> [[a]]
choices []       = [[]]
choices (xs:xss) = [c:cs | c <- xs, cs <- choices xss]

isSet :: Eq a => [a] -> Bool
isSet []      =  True
isSet (x:xs)  =  x `notElem` xs && isSet xs

-- NB. As the graph-isomorphism module applies representBy to potentially
-- large lists of graphs, computation is forced to ease memory pressure.
representBy :: (a->a->Bool) -> [a] -> [(a,Int)]
representBy equiv xs  =  foldl' add [] xs
  where
  add []             y  =  [(y,1)]
  add (xn@(x,n):xns) y  =  if x `equiv` y
                           then let n' = n+1 in n' `seq` (x,n'):xns
                           else let a' = add xns y in a' `seq` (xn : a')

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
blockZip []       []        =  Just []
blockZip (xs:xss) (ys:yss)  =  do
  guard (length xs == length ys)
  xyss <- blockZip xss yss
  return ((xs,ys) : xyss) 

