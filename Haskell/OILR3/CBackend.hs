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

-- TODO: should be in ProgCompile!
makeLoops :: OilrProg -> Maybe Int -> OilrProg -> OilrProg
makeLoops acc prev [] = reverse acc
makeLoops acc prev (i:is) = case (i, prev) of
    ( LUN n pr  , Nothing ) -> makeLoops (ORF:i:acc) (Just n) is
    ( LUN n pr  , Just p  ) -> makeLoops (ORB p:i:acc) (Just n) is
    ( XOE _ e _ , Just p  ) -> makeLoops (ORB p:i:acc) (Just e) is
    ( XIE _ e _ , Just p  ) -> makeLoops (ORB p:i:acc) (Just e) is
    ( LUE n _ _ , Just p  ) -> makeLoops (ORB p:i:acc) (Just p) is -- don't update the jump point!
    ( NEC n _   , Just p  ) -> makeLoops (ORB p:i:acc) (Just p) is
    ( LUE _ _ _ , Nothing ) -> error "Tried to match an edge before a node"
    ( XIE _ _ _ , Nothing ) -> error "Extend-in instruction cannot be first"
    ( XOE _ _ _ , Nothing ) -> error "Extend-out instruction cannot be first"
    _                       -> makeLoops (i:acc) prev is
        

progToC :: [Flag] -> [OilrProg] -> String
progToC flags iss = consts ++ cRuntime ++ predeclarations iss ++ concat defns
    where
        idx = makeSearchSpacesForDecl capBits oilr (concat iss)
        defns = map ((compileDefn idx) . (makeLoops [] Nothing) ) iss
        consts = "#define OILR_O_BITS (" ++ (show oBits) ++ ")\n"
              ++ "#define OILR_I_BITS (" ++ (show iBits) ++ ")\n"
              ++ "#define OILR_L_BITS (" ++ (show lBits) ++ ")\n"
              ++ "#define OILR_R_BITS (" ++ (show rBits) ++ ")\n"
              ++ case (EnableDebugging `elem` flags, EnableParanoidDebugging `elem` flags) of 
                    (False, False) -> "#define NDEBUG\n"
                    (_, True)      -> "#define OILR_PARANOID_CHECKS\n"
                    _              -> ""
              ++ if EnableExecutionTrace `elem` flags then "#define OILR_EXECUTION_TRACE\n" else ""
                    
        oilr@(oBits,iBits,lBits,rBits) = oilrBits capBits iss
        capBits = if DisableOilr `elem` flags then 0 else 8

-- Generate C declarations so that the ordering of definitions
-- doesn't matter
predeclarations :: [OilrProg] -> String
predeclarations iss = concatMap declare iss
    where
        declare ((PRO "Main"):_) = ""
        declare ((PRO s):_) = "\nvoid " ++ s ++ "();"
        declare ((RUL s):_) = "\nvoid " ++ s ++ "();"
        declare _ = error "Found an ill-formed definition"


oilrIndexTotalBits :: OilrIndexBits -> Int
oilrIndexTotalBits (o,i,l,r) = o+i+l+r

extractPredicates :: OilrProg -> [Pred]
extractPredicates is = concatMap harvestPred is
    where harvestPred (LUN _ p) = [p]
          harvestPred _         = []

oilrBits :: Int -> [OilrProg] -> OilrIndexBits
oilrBits cap iss = trace (show (f o, f i, f l, f' r)) (f o, f i, f l, f' r)
    -- We can't cap the r dimension, because there's no other check for the root flag.
    where
        f x = min cap $ f' x
        f' = (bits . maximum . map extract)
        (o, i, l, r) = unzip4 $ extractPredicates $ concat iss
        extract (Equ n) = n
        extract (GtE n) = n
        bits n = head $ dropWhile (\x -> 2^x <= n) [0,1..]

sigsForPred :: Int -> OilrIndexBits -> Pred -> (Pred, [Int])
sigsForPred cap (oBits, iBits, lBits, rBits) p@(o, i, l, r) =
    (p, nub [ o' `shift` oShift + i' `shift` iShift + l' `shift` lShift + r' `shift` rShift
                | o' <- case o of Equ n -> [min capSize n] ; GtE n -> [min capSize n..(1 `shift` oBits)-1]
                , i' <- case i of Equ n -> [min capSize n] ; GtE n -> [min capSize n..(1 `shift` iBits)-1]
                , l' <- case l of Equ n -> [min capSize n] ; GtE n -> [min capSize n..(1 `shift` lBits)-1]
                , r' <- case r of Equ n -> [n] ; GtE n -> [n..(1 `shift` rBits)-1] ])
    where
        capSize = (1 `shift` cap) - 1
        oShift = iShift + min cap iBits
        iShift = lShift + min cap lBits
        lShift = rShift + rBits
        rShift = 0


makeSearchSpacesForDecl :: Int -> OilrIndexBits -> OilrProg -> Mapping Pred [Int]
makeSearchSpacesForDecl cap bits is = map (sigsForPred cap bits) $ trace (show preds) preds
    where
        preds = ( nub . extractPredicates ) is



compileDefn :: Mapping Pred [Int] -> OilrProg -> String
compileDefn idx is = case head is of
    PRO _      -> concat (defines:cInstrs)
    RUL r      -> concat $ defines : (head cInstrs) : preamble r : (tail cInstrs)
    _          -> error "Definition doesn't begin with PRO or RUL instruction"
    where
        cInstrs = map (compileInstr idx) is
        defines = "\n#undef ABORT"
            ++ if nSlots > 0
                  then "\n#define ABORT do { trace(boolFlag?'s':'f'); unbindAll(matches, " ++ show nSlots ++ "); return ; } while (0)\n"
                  else "\n#define ABORT return"
        preamble id = "\n\tElement *matches[] = { NULL, " ++ slots ++ "};"
              ++ "\n\tDList   *state[]   = {" ++ slots ++ "};\n"
              ++ "#ifdef OILR_EXECUTION_TRACE\n\t oilrCurrentRule=" ++ show id ++ ";\n#endif\n"
        slots  = concat $ intersperse "," $ take nSlots $ repeat "NULL"
        nSlots = sum $ map countMatches is
        countMatches (LUN _ _)   = 1
        countMatches (LUE _ _ _) = 1
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

compileInstr _ RET           = "return;"
compileInstr m (CAL s)       = makeCFunctionCall s [] ++ compileInstr m ORF
compileInstr _ (ALP s)       = asLongAsPossible s []
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
compileInstr _ (LUE n src tgt) | src == tgt = 
                                    "\tdebug(\"In loop trav " ++ show n ++ "\\n\");\n"
                                    ++ makeCFunctionCallIntArgs "makeLoopTrav" [src, n]
                               | otherwise  =
                                    "\tdebug(\"In edge trav " ++ show n ++ "\\n\");\n"
                                    ++ makeCFunctionCallIntArgs "makeEdgeTrav" [src, n, tgt]
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
startCFunction name = concat [ "\nvoid ", name, "() {\n\tdebug(\"Entering ", name, "()\\n\");\n" ]

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
 

