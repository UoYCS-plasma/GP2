-- a simple implementation of extensible sparse arrays using ordered association lists
-- Colin Runciman (colin.runciman@york.ac.uk) April 2014

module ExAr (ExAr, empty, extend, idLookup, findAll, update, domain, removeAll, remove) where

import Prelude hiding (lookup)
import Data.Maybe (listToMaybe)

-- extensible sparse arrays
data ExAr a b = ExAr [(a,b)] Int deriving Show

-- intended data invariant for ExAr Int values
invExAr :: ExAr Int b -> Bool
invExAr ea@(ExAr _ i) = decreasing d && i > maximum d && minimum d >= 1
  where
  d = domain ea
  decreasing (x:y:etc)  =  x > y && decreasing (y:etc)
  decreasing _          =  True

empty :: ExAr a b
empty =  ExAr [] 1

extend :: ExAr Int b -> b -> (ExAr Int b, Int)
extend (ExAr ixs i) x  =  (ExAr ((i,x):ixs) (i+1), i)


-- used to lookup a node or edge in a graph.
idLookup :: Eq a => ExAr a b -> a -> Maybe b
idLookup (ExAr ixs _) id  =  listToMaybe [x | (k,x) <- ixs, k == id]

findAll :: (b -> Bool) -> ExAr a b -> [a]
findAll p (ExAr ixs _)  =  [id | (id,x) <- ixs, p x]

-- update outside domain is identity
update :: Eq a => (b->b) -> ExAr a b -> a -> ExAr a b
update f ea@(ExAr ixs i) id  =
  case suff of
  []          -> ea
  (_,x):suff' -> ExAr (pref ++ (id, f x):suff') i
  where
  (pref, suff)  =  span (\(j,_) -> j /= id) ixs

domain :: ExAr a b -> [a]
domain (ExAr ixs _)  =  [id | (id,_) <- ixs]

removeAll :: (b -> Bool) -> ExAr a b -> ExAr a b
removeAll p (ExAr ixs i)  =  ExAr (filter (\(id,x) -> not (p x)) ixs) i

-- removal outside domain is identity
remove :: Eq a => ExAr a b -> a -> ExAr a b
remove ea@(ExAr ixs i) id =
  case suff of
  []      -> ea
  _:suff' -> ExAr (pref ++ suff') i
  where
  (pref, suff)  =  span (\(k,_) -> k /= id) ixs

