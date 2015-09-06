module OILR3.CBackend (hostToC, progToC) where

import OILR3.Instructions
import OILR3.CRuntime

import Mapping

import Data.List
import Data.Bits
import Debug.Trace

type OilrProg = [Instr Int Int]
data OilrIndexBits = OilrIndexBits { oBits::Int, iBits::Int, lBits::Int, rBits::Int, cBits::Int} deriving (Show, Eq)

hostToC :: OilrProg -> String
hostToC is = makeCFunction "_HOST" $ map hostCompileInstruction is

hostCompileInstruction :: Instr Int Int -> String
hostCompileInstruction (ADN n)   = makeCFunctionCall "addNode" []
hostCompileInstruction (ADE e src tgt) = makeCFunctionCallIntArgs "addEdgeById" [src,tgt]
hostCompileInstruction (RTN n)   = makeCFunctionCallIntArgs "setRootById" [n]
hostCompileInstruction i         = error $ "Instruction " ++ show i ++ " not implemented for host graphs"

-- TODO: should be in ProgCompile!
makeLoops :: OilrProg -> Maybe Int -> OilrProg -> OilrProg
makeLoops acc prev [] = reverse acc
makeLoops acc prev (i:is) = case (i, prev) of
    ( LUN n pr  , Nothing ) -> makeLoops (ORF:i:acc) (Just n) is
    ( LUN n pr  , Just p  ) -> makeLoops (ORB p:i:acc) (Just n) is
    ( XOE _ e _ , Just p  ) -> makeLoops (ORB p:i:acc) (Just e) is
    ( XIE _ e _ , Just p  ) -> makeLoops (ORB p:i:acc) (Just e) is
    ( LUE n _ _ , Just p  ) -> makeLoops (ORB p:i:acc) (Just p) is -- don't update the jump point!
    ( LBE n _ _ , Just p  ) -> makeLoops (ORB p:i:acc) (Just p) is
    ( NEC n _   , Just p  ) -> makeLoops (ORB p:i:acc) (Just p) is
    ( LUE _ _ _ , Nothing ) -> error "Tried to match an edge before a node"
    ( LBE _ _ _ , Nothing ) -> error "Tried to match an edge before a node"
    ( XIE _ _ _ , Nothing ) -> error "Extend-in instruction cannot be first"
    ( XOE _ _ _ , Nothing ) -> error "Extend-out instruction cannot be first"
    _                       -> makeLoops (i:acc) prev is
        

progToC :: [Flag] -> [OilrProg] -> String
progToC flags iss = consts ++ cRuntime ++ predeclarations iss ++ concat defns
    where
        idx = makeSearchSpacesForDecl capBits oilr (concat iss)
        defns = map ((compileDefn (RecursiveRules `elem` flags) idx) . (makeLoops [] Nothing) ) iss
        consts = "#define OILR_O_BITS (" ++ (show $ oBits oilr) ++ ")\n"
              ++ "#define OILR_I_BITS (" ++ (show $ iBits oilr) ++ ")\n"
              ++ "#define OILR_L_BITS (" ++ (show $ lBits oilr) ++ ")\n"
              ++ "#define OILR_R_BITS (" ++ (show $ rBits oilr) ++ ")\n"
              ++ "#define OILR_C_BITS (" ++ (show $ cBits oilr) ++ ")\n"
              ++ case (EnableDebugging `elem` flags, EnableParanoidDebugging `elem` flags) of 
                    (False, False) -> "#define NDEBUG\n"
                    (_, True)      -> "#define OILR_PARANOID_CHECKS\n"
                    _              -> ""
              ++ if EnableExecutionTrace `elem` flags then "#define OILR_EXECUTION_TRACE\n" else ""
                    
        oilr = oilrBits capBits iss
        capBits = if DisableOilr `elem` flags then 0 else 8

-- Generate C declarations so that the ordering of definitions
-- doesn't matter
predeclarations :: [OilrProg] -> String
predeclarations iss = concatMap declare iss
    where
        declare ((PRO "Main"):_) = ""
        declare ((PRO s):_) = "\nvoid " ++ s ++ "(long recursive);"
        declare ((RUL s):_) = "\nvoid " ++ s ++ "(long recursive);"
        declare _ = error "Found an ill-formed definition"


oilrIndexTotalBits :: OilrIndexBits -> Int
oilrIndexTotalBits (OilrIndexBits o i l r c) = o+i+l+r+c

extractPredicates :: OilrProg -> [Pred]
extractPredicates is = concatMap harvestPred is
    where harvestPred (LUN _ p) = [p]
          harvestPred _         = []

oilrBits :: Int -> [OilrProg] -> OilrIndexBits
oilrBits cap iss = OilrIndexBits (f o) (f i) (f l) (f' r) (f c)
    -- We can't cap the r dimension, because there's no other check for the root flag.
    where
        f x = min cap $ f' x
        f' = (bits . maximum . map extract)
        (o, i, l, r, c) = unzip5 $ explodePreds $ extractPredicates $ concat iss
        explodePreds :: [Pred] -> [(Dim, Dim, Dim, Dim, Dim)]
        explodePreds prs = [ (oDim pr, iDim pr, lDim pr, rDim pr, cDim pr) | pr <- prs ]
        extract (Equ n) = n
        extract (GtE n) = n
        bits n = head $ dropWhile (\x -> 2^x <= n) [0,1..]

sigsForPred :: Int -> OilrIndexBits -> Pred -> (Pred, [Int])
sigsForPred cap bits pr =
    (pr, nub [ o' `shift` oShift + i' `shift` iShift + l' `shift` lShift + r' `shift` rShift
                | o' <- sigForDim (oBits bits) o
                , i' <- sigForDim (iBits bits) i
                , l' <- sigForDim (lBits bits) l
                , r' <- sigForDim (rBits bits) r ])
    where
        ( o, i, l, r ) = (oDim pr, iDim pr, lDim pr, rDim pr)
        capSize = (1 `shift` cap) - 1
        oShift = iShift + (min cap $ iBits bits)
        iShift = lShift + (min cap $ lBits bits)
        lShift = rShift + rBits bits
        rShift = 0

        sigForDim :: Int -> Dim -> [Int]
        sigForDim _ (Equ n) = [ min capSize n ]
        sigForDim b (GtE n) = [ min capSize n .. ( 1 `shift` b)-1 ]


makeSearchSpacesForDecl :: Int -> OilrIndexBits -> OilrProg -> Mapping Pred [Int]
makeSearchSpacesForDecl cap bits is = map (sigsForPred cap bits) $ trace (show preds) preds
    where
        preds = ( nub . extractPredicates ) is



compileDefn :: Bool -> Mapping Pred [Int] -> OilrProg -> String
compileDefn recurse idx is = case head is of
    PRO _      -> concat (defines False "":cInstrs)
    RUL r      -> concat $ defines recurse r : (head cInstrs) : preamble r : (tail cInstrs)
    _          -> error "Definition doesn't begin with PRO or RUL instruction"
    where
        cInstrs = map (compileInstr idx) is
        defines rec id = "\n#undef ABORT\n#undef RECURSE"
            ++ "\n#define ABORT "
            ++ ( if nSlots > 0
                  then "do { trace(boolFlag?'s':'f'); unbindAll(matches, " ++ show nSlots ++ "); return ; } while (0)\n"
                  else "return" )
            ++ ( "\n#define RECURSE " ++ if rec then id ++ "(1)" else "" )
        preamble id = "\n\tElement *matches[] = { NULL, " ++ slots ++ "};"
              ++ "\n\tDList   *state[]   = {" ++ slots ++ "};\n"
              ++ "#ifdef OILR_EXECUTION_TRACE\n\t oilrCurrentRule=" ++ show id ++ ";\n#endif\n"
        slots  = concat $ intersperse "," $ take nSlots $ repeat "NULL"
        nSlots = sum $ map countMatches is
        countMatches (LUN _ _)   = 1
        countMatches (LUE _ _ _) = 1
        countMatches (LBE _ _ _) = 1
        countMatches (XOE _ _ _) = 2
        countMatches (XIE _ _ _) = 2
        countMatches (ADN _)     = 1
        countMatches (ADE _ _ _) = 1
        countMatches _           = 0

makeModifyAndBind :: Int -> String -> [Int] -> String
makeModifyAndBind i fun args = "\t" ++ matchInd i ++ " = " ++ makeCFunctionCall fun (map matchInd args)
    where
        matchInd i = "matches[" ++ show i ++ "]"


compileInstr :: Mapping Pred [Int] -> Instr Int Int -> String
compileInstr _ (ADN n)         = makeModifyAndBind n "addNode" []
compileInstr _ (ADE e src tgt) | src==tgt  = makeModifyAndBind e "addLoop" [src]
                               | otherwise = makeModifyAndBind e "addEdge" [src, tgt]
compileInstr _ (RTN n)         = "\tsetRoot(matches[" ++ show n ++ "]);\n"
compileInstr _ (URN n)         = "\tunsetRoot(matches[" ++ show n ++ "]);\n"
compileInstr _ (DEN n)         = "\tdeleteNode(matches[" ++ show n ++ "]);\n"
compileInstr _ (DEE e)         = "\tdeleteEdge(matches[" ++ show e ++ "]);\n"

compileInstr _ OK            = "\tif (recursive) { RECURSE; boolFlag=1; }\n"
compileInstr _ RET           = "return;"
compileInstr m (CAL s)       = makeCFunctionCall s ["0"] ++ compileInstr m ORF
compileInstr _ (ALP s)       = asLongAsPossible s [1]
compileInstr _ ORF           = "\tif (!boolFlag) ABORT ;\n"
compileInstr _ (ORB n)       = "\tif (!boolFlag) goto " ++ labelFor n ++ ";\n"

compileInstr _ (PRO "Main")  = startCFunction "_GPMAIN"
compileInstr _ (PRO s)       = startCFunction s
compileInstr _ (RUL s)       = startCFunction s
compileInstr _ UBA           = "\tABORT;\n"
compileInstr _ END           = endCFunction 

-- compileInstr (CRS n sig)     = makeCFunctionCallIntArgs "resetTrav" [n] -- TODO
compileInstr idx (LUN n sig) = labelFor n
    ++ ":\n\tdebug(\"In LUN trav " ++ show n ++ "\\n\");"
    ++ "\n\tcheckGraph();\n"
    ++ case definiteLookup sig idx of
        []  -> error "Can't find a node in an empty search space!"
        [s] -> makeCFunctionCall "makeSimpleTrav"
                        [show n, "index(" ++ show s ++ ")" ]
        ss  -> makeCFunctionCall "makeTrav" 
                        (show n:show (length ss):map (\s ->  "{0, index(" ++ show s ++ ")}") ss )
compileInstr _ (LUE n src tgt)
    | src == tgt = "\tdebug(\"In loop trav " ++ show n ++ "\\n\");\n"
                ++ makeCFunctionCallIntArgs "makeLoopTrav" [src, n]
    | otherwise  = "\tdebug(\"In edge trav " ++ show n ++ "\\n\");\n"
                ++ makeCFunctionCallIntArgs "makeEdgeTrav" [src, n, tgt]
compileInstr _ (LBE n a b) = "\tdebug(\"In bidi trav " ++ show n ++ "\\n\");\n"
                          ++ makeCFunctionCallIntArgs "makeBidiEdgeTrav" [a, n, b]
compileInstr _ (XOE src e tgt) = labelFor e
    ++ ":\n\tdebug(\"In XOE trav " ++ show e ++ "\\n\");\n"
    ++ makeCFunctionCallIntArgs "makeExtendOutTrav" [src, e, tgt, 1]
compileInstr _ (XIE src e tgt) = labelFor e
    ++ ":\n\tdebug(\"In XIE trav " ++ show e ++ "\\n\");\n"
    ++ makeCFunctionCallIntArgs "makeExtendInTrav" [src, e, tgt, 1]
compileInstr _ (NEC src tgt)   = makeCFunctionCallIntArgs "makeAntiEdgeTrav" [src, tgt]


makeCFunction :: String -> [String] -> String
makeCFunction name lines = concat [startCFunction name,  body, "\n}\n"]
    where
        body = intercalate "\n\t" lines

startCFunction :: String -> String
startCFunction name = concat [ "\nvoid ", name, "(long recursive) {\n\tdebug(\"Entering ", name, "()\\n\");\n" ]

endCFunction :: String
endCFunction = "}\n"

asLongAsPossible :: String -> [Int] -> String
asLongAsPossible fname args = concat [ "\tdo {\n\toilrReport();\n\t", makeCFunctionCallIntArgs fname args, "\t} while (boolFlag);\n\tboolFlag=1;\n" ]

makeCFunctionCallIntArgs :: String -> [Int] -> String
makeCFunctionCallIntArgs fname args = makeCFunctionCall fname $ map show args

makeCFunctionCall :: String -> [String] -> String
makeCFunctionCall fname args = concat [ ('\t':fname) , "(", argStr, ");\n" ]
    where
        argStr = intercalate ", " args

labelFor :: Int -> String
labelFor n = "trav_no_" ++ show n
 

