-- An implementation of extensible sparse arrays using Data.Map.Lazy
-- Colin Runciman (colin.runciman@york.ac.uk) April 2014

module ExAr (ExAr, ExAr.empty, extend, idLookup, findAll, ExAr.update, domain, removeAll, remove) where

import Prelude hiding (lookup, filter)
import Data.Maybe (listToMaybe)
import Data.Map.Lazy

-- extensible sparse arrays
data ExAr a b = ExAr (Map a b) Int

instance (Show a, Show b) => Show (ExAr a b) where
  show (ExAr m i)  =  show (toList m) ++ " " ++ show i

empty :: ExAr a b
empty = ExAr Data.Map.Lazy.empty 1

extend :: ExAr Int b -> b -> (ExAr Int b, Int)
extend (ExAr m i) x  =  (ExAr (insert i x m) (i+1), i)

-- used to lookup value at an index
idLookup :: Ord a => ExAr a b -> a -> Maybe b
idLookup (ExAr m _) k  =  lookup k m

findAll :: (b -> Bool) -> ExAr a b -> [a]
findAll p (ExAr m _)  =  keys $ filter p m

-- update outside domain is identity
update :: Ord a => (b->b) -> ExAr a b -> a -> ExAr a b
update f (ExAr m i) k  =  ExAr (adjust f k m) i

domain :: ExAr a b -> [a]
domain (ExAr m _)  =  keys m

removeAll :: (b -> Bool) -> ExAr a b -> ExAr a b
removeAll p (ExAr m i)  =  ExAr (filter (not . p) m) i

-- removal outside domain is identity
remove :: Ord a => ExAr a b -> a -> ExAr a b
remove (ExAr m i) k  =  ExAr (delete k m) i

