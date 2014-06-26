-- a simple implementation of extensible sparse arrays using ordered association lists
-- Colin Runciman (colin.runciman@york.ac.uk) April 2014
-- Modifications by Chris Bak 26 June 2014

module ExAr (ExAr, empty, extend, idLookup, listLookup, findAll, update, domain, removeAll, remove) where

import Prelude hiding (lookup)
import Data.Maybe (listToMaybe)

-- extensible sparse arrays

-- ExAr is an array of items indexed by String identifiers.
-- Used for nodes, edges and symbols.
-- Not sure if the extra Int of the old definition below is required here,
-- as it was used previously to keep a unique identifier available for new
-- nodes and edges.
-- data ExAr a b = ExAr [(a,b)] Int
data ExAr a = ExAr [(String,a)] deriving Show

-- intended data invariant for ExAr Int values
{-invExAr :: ExAr Int b -> Bool
invExAr ea@(ExAr _ i) = decreasing d && i > maximum d && minimum d >= 1
  where
  d = domain ea
  decreasing (x:y:etc)  =  x > y && decreasing (y:etc)
  decreasing _          =  True-}

empty :: ExAr a
empty =  ExAr []

-- used only in Graph module for adding new nodes and edges. 
extend :: ExAr a -> String -> a -> (ExAr a, String)
extend (ExAr xs) s x =  (ExAr ((s,x):xs), s)

-- Now addSymbol and extend do the same thing.
-- addSymbol :: ExAr a b -> a -> b -> ExAr a b
-- addSymbol (ExAr ixs i) id val = (ExAr ((id,val):ixs) i)

-- used to lookup a node or edge within a single graph, a situation where
-- there should only be one multiple node/edge with a particular ID.
idLookup :: ExAr a -> String -> Maybe a
idLookup (ExAr xs) id  =  listToMaybe [x | (s,x) <- xs, s == id]

-- used to look up a list of symbols in the symbol table.
-- In comparison to the above, there may be more than one symbol with
-- the same id.
listLookup :: ExAr a -> String -> [a]
listLookup (ExAr xs) id  =  [x | (s,x) <- xs, s == id]

findAll :: (a -> Bool) -> ExAr a -> [String]
findAll p (ExAr xs)  =  [id | (id,x) <- xs, p x]

-- update outside domain is identity
update :: (a -> a) -> ExAr a -> String -> ExAr a
update f ea@(ExAr xs) id  =
  case suff of
  []          -> ea
  (_,x):suff' -> ExAr (pref ++ (id, f x):suff')
  where
  (pref, suff)  =  span (\(k,_) -> k /= id) xs

domain :: ExAr a -> [String]
domain (ExAr xs)  =  [id | (id,_) <- xs]

removeAll :: (a -> Bool) -> ExAr a -> ExAr a
removeAll p (ExAr xs)  =  ExAr (filter (\(id,x) -> not (p x)) xs)

-- removal outside domain is identity
remove :: ExAr a -> String -> ExAr a
remove ea@(ExAr xs) id =
  case suff of
  []      -> ea
  _:suff' -> ExAr (pref ++ suff')
  where
  (pref, suff)  =  span (\(k,_) -> k /= id) xs

