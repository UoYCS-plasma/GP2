module OILR4.Spaces where

import OILR4.Config
import OILR4.IR


allIndices :: [Sig]
allIndices = [ Sig o i l r | b <- [True, False]
                           , c <- [0..(cBits indBits - 1)]
                           , o <- [0..(oBits indBits - 1)]
                           , i <- [0..(iBits indBits - 1)]
                           , l <- [0..(lBits indBits - 1)]
                           , r <- [True, False] ]

-- Compile search-spaces from IR
makeSpc :: OilrMod OilrElem -> Spc
makeSpc (Same n)     = spaceForInterface n
makeSpc (Change n _) = spaceForInterface n
makeSpc (Create _)   = []
makeSpc (Delete n)   = spaceForNonIF n
-- makeSpc (Check _)    = error "don't know what to do"
makeSpc x            = error $ "Don't know how to make a space from " ++ show x

-- data OilrMod a = Same a  |  Change a a  | Create a | Delete a | Check a  deriving (Show, Eq)

spaceForInterface :: OilrElem -> Spc
spaceForInterface (IRNode _ _ _ sig) = [ s | s <- allIndices , s `compatible` sig ] 
    -- is the first signature contained in the search space of the second?
    where (Sig o' i' l' r') `compatible` (Sig o i l r) = o' >= o
                                                      && i' >= i
                                                      && l' >= l
                                                      && r' >= r

spaceForNonIF :: OilrElem -> Spc
spaceForNonIF (IRNode _ _ _ sig) = [sig]


