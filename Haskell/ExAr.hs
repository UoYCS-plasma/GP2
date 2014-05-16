-- a simple implementation of extensible sparse arrays using ordered association lists
-- Colin Runciman (colin.runciman@york.ac.uk) April 2014

module ExAr (ExAr, empty, extend, lookup, findAll, update, domain, removeAll, remove) where

import Prelude hiding (lookup)
import Data.Maybe (listToMaybe)

-- extensible sparse arrays
data ExAr a = ExAr [(Int,a)] Int deriving Show

-- intended data invariant for ExAr values
invExAr :: ExAr a -> Bool
invExAr ea@(ExAr _ i) = decreasing d && i > maximum d && minimum d >= 1
  where
  d = domain ea
  decreasing (x:y:etc)  =  x > y && decreasing (y:etc)
  decreasing _          =  True

empty :: ExAr a
empty  =  ExAr [] 1

extend :: ExAr a -> a -> (ExAr a, Int)
extend (ExAr ixs i) x  =  (ExAr ((i,x):ixs) (i+1), i)

lookup :: ExAr a -> Int -> Maybe a
lookup (ExAr ixs _) i  =  listToMaybe [x | (j,x) <- ixs, j==i]

findAll :: (a -> Bool) -> ExAr a -> [Int]
findAll p (ExAr ixs _)  =  [i | (i,x) <- ixs, p x]

-- update outside domain is identity
update :: (a->a) -> ExAr a -> Int -> ExAr a
update f ea@(ExAr ixs i') i  =
  case suff of
  []          -> ea
  (_,x):suff' -> ExAr (pref ++ (i, f x):suff') i'
  where
  (pref, suff)  =  span (\(j,_) -> j /= i) ixs

domain :: ExAr a -> [Int]
domain (ExAr ixs _)  =  [i | (i,_) <- ixs]

removeAll :: (a -> Bool) -> ExAr a -> ExAr a
removeAll p (ExAr ixs i')  =  ExAr (filter (\(i,x) -> not (p x)) ixs) i'

-- removal outside domain is identity
remove :: ExAr a -> Int -> ExAr a
remove ea@(ExAr ixs i') i   =
  case suff of
  []      -> ea
  _:suff' -> ExAr (pref ++ suff') i'
  where
  (pref, suff)  =  span (\(j,_) -> j /= i) ixs

