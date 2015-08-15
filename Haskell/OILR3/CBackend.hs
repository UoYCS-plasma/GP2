module OILR3.CBackend (hostToC, progToC) where

import OILR3.Instructions
import OILR3.CRuntime

import Mapping

import Data.List
import Data.Bits
import Debug.Trace

type OilrProg = [Instr Int Int]
type OilrIndexBits = (Int, Int, Int, Int)

hostToC :: OilrProg -> String
hostToC is = makeCFunction "_HOST" $ map hostCompileInstruction is

hostCompileInstruction :: Instr Int Int -> String
hostCompileInstruction (ADN n)   = makeCFunctionCall "addNode" []
hostCompileInstruction (ADE e src tgt) = makeCFunctionCallIntArgs "addEdgeById" [src,tgt]
hostCompileInstruction (RTN n)   = makeCFunctionCallIntArgs "setRootById" [n]
hostCompileInstruction i         = error $ "Instruction " ++ show i ++ " not implemented for host graphs"

makeLoops :: OilrProg -> Maybe Int -> OilrProg -> OilrProg
makeLoops acc prev [] = reverse acc
makeLoops acc prev (i:is) = case (i, prev) of
    ( LUN n pr  , Nothing ) -> makeLoops (ORF:i:acc) (Just n) is
    ( LUN n pr  , Just p  ) -> makeLoops (ORB p:i:acc) (Just n) is
    ( LUE n _ _ , Just p  ) -> makeLoops (ORB p:i:acc) (Just p) is -- don't update the jump point!
    ( LUE _ _ _ , Nothing ) -> error "Tried to match an edge before a node"
    _                       -> makeLoops (i:acc) prev is
        

progToC :: Int -> [OilrProg] -> String
progToC travCount iss = consts ++ searchSpaces ++ cRuntime ++ predeclarations iss ++ concat defns
    where
        searchSpaces = compileSearchSpaces [ (i, is)
                                           | (i, is) <- zip [1..] $ map snd idx ]
        idx = makeSearchSpacesForDecl oilr (concat iss)
        defns = map ((compileDefn idx) . (makeLoops [] Nothing) ) iss
        consts = "#define OILR_O_BITS (" ++ (show oBits) ++ ")\n"
              ++ "#define OILR_I_BITS (" ++ (show iBits) ++ ")\n"
              ++ "#define OILR_L_BITS (" ++ (show lBits) ++ ")\n"
              ++ "#define OILR_R_BITS (" ++ (show rBits) ++ ")\n"
              -- ++ "#define TRAV_COUNT (" ++ show travCount ++ ")\n"
        oilr@(oBits,iBits,lBits,rBits) = oilrBits iss

-- Generate C declarations so that the ordering of definitions
-- doesn't matter
predeclarations :: [OilrProg] -> String
predeclarations iss = concatMap declare iss
    where
        declare ((DEF "Main"):_) = ""
        declare ((DEF s):_) = "\nvoid " ++ s ++ "();"
        declare _ = error "Found an ill-formed definition"


oilrIndexTotalBits :: OilrIndexBits -> Int
oilrIndexTotalBits (o,i,l,r) = o+i+l+r

extractPredicates :: OilrProg -> [Pred]
extractPredicates is = concatMap harvestPred is
    where harvestPred (LUN _ p) = [p]
          harvestPred _         = []

oilrBits :: [OilrProg] -> OilrIndexBits
oilrBits iss = trace (show (f o, f i, f l, f r)) (f o, f i, f l, f r)
    where
        f = (bits . maximum . map extract) 
        (o, i, l, r) = unzip4 $ extractPredicates $ concat iss
        extract (Equ n) = n
        extract (GtE n) = n
        bits n = head $ dropWhile (\x -> 2^x < n) [0,1..]

sigsForPred :: OilrIndexBits -> Pred -> (Pred, [Int])
sigsForPred (oBits, iBits, lBits, rBits) p@(o, i, l, r) =
    (p, nub [ o' `shift` oShift + i' `shift` iShift + l' `shift` lShift + r' `shift` rShift
                | o' <- case o of Equ n -> [n] ; GtE n -> [n..(1 `shift` oBits)]
                , i' <- case i of Equ n -> [n] ; GtE n -> [n..(1 `shift` iBits)]
                , l' <- case l of Equ n -> [n] ; GtE n -> [n..(1 `shift` lBits)]
                , r' <- case r of Equ n -> [n] ; GtE n -> [n..(1 `shift` rBits)] ])
    where
        rShift = 0
        lShift = rShift + rBits
        iShift = lShift + lBits
        oShift = iShift + iBits


makeSearchSpacesForDecl :: OilrIndexBits -> OilrProg -> Mapping Pred [Int]
makeSearchSpacesForDecl bits is = map (sigsForPred bits) $ trace (show preds) preds
    where
        preds = ( nub . extractPredicates ) is


compileSearchSpaces :: [ (Int, [Int]) ] -> String
compileSearchSpaces ss = concatMap makeSpace ss ++ "long *searchSpaces[] = { " ++ concatMap makeSpaces ss ++ "};\n\n"
    where
        makeSpace (p, is) = "long search_space_" ++ show p ++ "[] = {" ++ ( concat $ intersperse ", " $  map show is ) ++ "};\n"
        makeSpaces (p, is) = "search_space_" ++ show p ++ ", "


compileDefn :: Mapping Pred [Int] -> OilrProg -> String
compileDefn idx is = case head is of
    DEF "Main" -> concat cInstrs
    DEF _      -> concat $ head cInstrs : preamble : tail cInstrs
    _          -> error "Definition doesn't begin with DEF instruction"
    where
        cInstrs = map (compileInstr idx) is
        preamble = "\tElement *matches[] = {" ++ slots
            ++ "};\n\tDList   *state[]   = {" ++ slots ++ "};\n"
        slots = concat $ intersperse "," $ filter (/="") $ map countMatches is
        countMatches (LUN _ _)   = "NULL"
        countMatches (LUE _ _ _) = "NULL"
        countMatches (ADN _)     = "NULL"
        countMatches (ADE _ _ _) = "NULL"
        countMatches _           = ""

makeModifyAndBind :: Int -> String -> [Int] -> String
makeModifyAndBind i fun args = "\t" ++ matchInd i ++ " = " ++ makeCFunctionCall fun (map matchInd args)
    where
        matchInd i = "matches[" ++ show i ++ "]"


compileInstr :: Mapping Pred [Int] -> Instr Int Int -> String
compileInstr _ (ADN n)         = makeModifyAndBind n "addNode" []
compileInstr _ (ADE e src tgt) = makeModifyAndBind e "addEdge" [src, tgt]
compileInstr _ (RTN n)         = "setRoot(matches[" ++ show n ++ "]);\n"
compileInstr _ (URN n)         = "unsetRoot(matches[" ++ show n ++ "]);\n"
compileInstr _ (DEN n)         = "deleteNode(matches[" ++ show n ++ "]);\n"
compileInstr _ (DEE e)         = "deleteEdge(matches[" ++ show e ++ "]);\n"

compileInstr _ RET           = "return;"
compileInstr _ (CAL s)       = makeCFunctionCall s []
compileInstr _ (ALP s)       = asLongAsPossible s []
compileInstr _ ORF           = "if (!boolFlag) return ;\n"
compileInstr _ (ORB n)       = "if (!boolFlag) goto " ++ labelFor n ++ ";\n"

compileInstr _ (DEF "Main")  = startCFunction "_GPMAIN"
compileInstr _ (DEF s)       = startCFunction s
compileInstr _ END           = endCFunction 

-- compileInstr (CRS n sig)     = makeCFunctionCallIntArgs "resetTrav" [n] -- TODO
compileInstr idx (LUN n sig) = labelFor n  ++ ": "
    ++ case definiteLookup sig idx of
        []  -> error "Can't find a node in an empty search space!"
        [s] -> makeCFunctionCall "makeSimpleTrav"
                        [show n, makeCFunctionCallIntArgs "index" [s] ]
        ss  -> makeCFunctionCall "makeTrav" 
                        (show n:map (\s ->  "index(" ++ show s ++ ")") ss)
compileInstr _ (LUE n src tgt) = makeCFunctionCallIntArgs "makeEdgeTrav" [n, src, tgt]



makeCFunction :: String -> [String] -> String
makeCFunction name lines = concat [startCFunction name,  body, "\n}\n"]
    where
        body = intercalate "\n\t" lines

startCFunction :: String -> String
startCFunction name = concat [ "\nvoid ", name, "() {\n\t" ]

endCFunction :: String
endCFunction = "}\n"

asLongAsPossible :: String -> [Int] -> String
asLongAsPossible fname args = concat [ "do {\n", makeCFunctionCallIntArgs fname args, "} while (boolFlag);\n" ]

makeCFunctionCallIntArgs :: String -> [Int] -> String
makeCFunctionCallIntArgs fname args = makeCFunctionCall fname $ map show args

makeCFunctionCall :: String -> [String] -> String
makeCFunctionCall fname args = concat [ fname , "(", argStr, ");\n" ]
    where
        argStr = intercalate ", " args

labelFor :: Int -> String
labelFor n = "trav_no_" ++ show n
 

