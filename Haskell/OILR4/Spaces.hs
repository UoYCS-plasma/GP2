module OILR4.Spaces where

import OILR4.Config
import OILR4.IR

import GPSyntax
import Mapping

import Data.List
import Data.Bits

import Debug.Trace


allIndices :: [Sig]
allIndices = [ Sig o i l r | b <- [True, False]
                           , c <- [0..(cBits indBits - 1)]
                           , o <- [0..(oBits indBits - 1)]
                           , i <- [0..(iBits indBits - 1)]
                           , l <- [0..(lBits indBits - 1)]
                           , r <- [True, False] ]


bShift i = shift i $ rBits indBits + lBits indBits + iBits indBits + oBits indBits + cBits indBits
cShift i = shift i $ rBits indBits + lBits indBits + iBits indBits + oBits indBits
oShift i = shift i $ rBits indBits + lBits indBits + iBits indBits
iShift i = shift i $ rBits indBits + lBits indBits
lShift i = shift i $ rBits indBits
rShift i = i


analyseSS :: Mapping  Int [Ind] -> [Ind]
analyseSS ss = {- trace (show $ differences) -} []
    where intersections = [ s1 `intersect` s2 | (_,s1) <- ss, (_,s2) <- ss, s1/=s2 ]
          differences   = [ s1 \\ s2 | (_,s1) <- ss, s2 <- intersections ]

makePackedSpaces :: OilrConfig -> OilrConfig
makePackedSpaces cf = cf { logicalToPhys=l2p, packedSpaces=s2p, physIndCount=length packed }
    where packed = zip [0..] $ packIndices cf
          l2p = [ (l, p) | (p, (_, ls)) <- packed, l <- ls ]
          s2p = [ (s, ps) | (s, ls) <- searchSpaces cf, let ps = nub $ map lToP ls ]
          lToP l = definiteLookup l l2p

packIndices :: OilrConfig -> Mapping [Int] [Ind]
packIndices cf = ([], unusedInds (indexCount cf) ss) : disjointSpaces [] ss 
    where ss = searchSpaces cf

unusedInds :: Int -> Mapping Int [Ind] -> [Int]
unusedInds n ss = [0..n-1] \\ all
    where all = concat $ map snd ss

disjointSpaces :: [([Int],[Ind])] -> Mapping Int [Ind] -> [([Int],[Ind])]
disjointSpaces acc ((i,s):ss) = disjointSpaces acc' ss
    where acc' = s':concat [ints, difs]
          -- the intersections between s and the acc'd spaces
          ints = [ (i:is, d `intersect` s) | (is,d) <- acc ]
          -- remove members of s from existing acc'd spaces
          difs = [ (is, d \\ s) | (is,d) <- acc ]
          -- remove members of all existing acc'd spaces from s
          s'   = ([i], s \\ (nub $ concatMap snd acc))
disjointSpaces acc [] = sortBy (\a b -> (length.fst) a `compare` (length.fst) b) $ filter (not.null.snd) acc


    
-- Compile list of valid indices to search for a given modification
makeSpc :: OilrConfig -> OilrMod -> OilrConfig
makeSpc cfg (Same n)     = updateSpcMapping cfg $ indexIdsForNode n True
makeSpc cfg (Change n _) = updateSpcMapping cfg $ indexIdsForNode n True
makeSpc cfg (Create _)   = cfg
makeSpc cfg (Delete n)   = updateSpcMapping cfg $ indexIdsForNode n False
-- makeSpc cfg (Check _)    = error "don't know what to do"
makeSpc cfg x            = error $ "Don't know how to make a space from " ++ show x

updateSpcMapping :: OilrConfig -> [Ind] -> OilrConfig
updateSpcMapping cfg new = cfg { searchSpaces = ((spcId, new):existing) }
    where existing = searchSpaces cfg
          spcId    = length existing


-- data OilrMod a = Same a  |  Change a a  | Create a | Delete a | Check a  deriving (Show, Eq)

indexIdsForNode :: OilrElem -> Bool -> [Ind]
indexIdsForNode (IRNode _ clr lbl sig) isIF = indexIds lbl clr sig isIF

indexIds :: IRLabel -> Colour -> Sig -> Bool -> [Ind]
indexIds lbl clr sig isIF = [ l+c+s | l <- indsOfLabel lbl
                                    , c <- indsOfColour clr
                                    , s <- indsOfSig isIF sig ]

indsOfLabel :: IRLabel -> [Int]
indsOfLabel IRAny     = map bShift [0, 1]
indsOfLabel (IRVar _) = map bShift [0, 1]
indsOfLabel (IRLst _) = map bShift [0, 1]
indsOfLabel (IRInt _) = [bShift 1]
indsOfLabel IREmpty   = [bShift 0]

indsOfColour :: Colour -> [Int]
indsOfColour Any  = [ cShift id | (_, id) <- colourIds ]
indsOfColour c    = [ cShift $ definiteLookup c colourIds ]

indsOfSig :: Bool -> Sig -> [Int]
indsOfSig False (Sig o i l r) = [ oil + rShift r' | r' <- [0,1], r' >= toInt r ]
    where oil = oShift o + iShift i + lShift l
indsOfSig True  (Sig o i l r) = [ oShift o' + iShift i' + lShift l' + rShift r'
                                | o' <- getInds o $ oBits indBits
                                , i' <- getInds i $ iBits indBits
                                , l' <- getInds l $ lBits indBits
                                , r' <- getInds (toInt r) $ rBits indBits ]

getInds actual bits =
    if actual > max
        then [max]
        else [actual..max] 
    where max = (1 `shift` bits) - 1

toInt :: Bool -> Int
toInt True  = 1
toInt False = 0

