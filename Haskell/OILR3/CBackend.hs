module OILR3.CBackend (hostToC, progToC) where

import OILR3.Instructions
import OILR3.CRuntime

import Mapping

import Data.List
import Data.Bits
import Debug.Trace

type OilrProg = [Instr Int Int]
data OilrIndexBits = OilrIndexBits { oBits::Int, iBits::Int, lBits::Int, rBits::Int, cBits::Int} deriving (Show, Eq)

data OilrProps = OilrProps { flags   :: [Flag]
                           , spaces  :: Mapping Pred [Int]
                           , oilrInd :: OilrIndexBits
                           , stateMask :: Mapping String [Int] 
                           , nSlots   :: Mapping String Int }


hostToC :: OilrProg -> String
hostToC is = makeCFunction "_HOST" $ map hostCompileInstruction is

hostCompileInstruction :: Instr Int Int -> String
hostCompileInstruction (ADN n)   = makeCFunctionCall "addNode" []
hostCompileInstruction (ADE e src tgt) = makeCFunctionCallIntArgs "addEdgeById" [src,tgt]
hostCompileInstruction (RTN n)   = makeCFunctionCallIntArgs "setRootById" [n]
hostCompileInstruction i         = error $ "Instruction " ++ show i ++ " not implemented for host graphs"


progToC :: [Flag] -> [OilrProg] -> String
progToC flags decls = consts ++ cRuntime ++ cCode
    where
        oilr = analyseProg flags decls
        cCode = intercalate "\n" $ prototypes decls
                                ++ concatMap (compileDefn oilr) decls
        ind = oilrInd oilr
        consts = concat [ "#define OILR_O_BITS (" ++ (show $ oBits ind) ++ ")\n"
                        , "#define OILR_I_BITS (" ++ (show $ iBits ind) ++ ")\n"
                        , "#define OILR_L_BITS (" ++ (show $ lBits ind) ++ ")\n"
                        , "#define OILR_R_BITS (" ++ (show $ rBits ind) ++ ")\n"
                        , "#define OILR_C_BITS (" ++ (show $ cBits ind) ++ ")\n"
                        , case ( EnableDebugging         `elem` flags
                               , EnableParanoidDebugging `elem` flags) of 
                            (False, False) -> "#define NDEBUG\n"
                            (_, True)      -> "#define OILR_PARANOID_CHECKS\n"
                            _              -> ""
                        , if EnableExecutionTrace `elem` flags
                            then "#define OILR_EXECUTION_TRACE\n"
                            else ""  ]

analyseProg :: [Flag] -> [OilrProg] -> OilrProps
analyseProg fs decls = OilrProps { flags = fs , spaces = spcs , oilrInd = bits
                                 , nSlots = slots, stateMask = mask }
    where
        spcs = makeSearchSpaces capBits bits $ concat decls
        bits = oilrBits capBits decls
        capBits = if DisableOilr `elem` fs then 0 else 8
        slots = map makeSlots decls
        mask  = map makeMask decls

makeSlots :: OilrProg -> (String, Int)
makeSlots is = ( idFor is , sum $ map countMatches is )
    where
        countMatches (LUN _ _)   = 1
        countMatches (LUE _ _ _) = 1
        countMatches (LBE _ _ _) = 1
        countMatches (XOE _ _ _) = 2
        countMatches (XIE _ _ _) = 2
        countMatches (ADN _)     = 1
        countMatches (ADE _ _ _) = 1
        countMatches _           = 0

makeMask :: OilrProg -> (String, [Int])
makeMask is = ( idFor is , concatMap mask is )
    where
        mask (LUN _ _)   = [-1]
        mask (LUE _ _ _) = [0]
        mask (LBE _ _ _) = [0]
        mask (XOE _ _ _) = [0, 0]
        mask (XIE _ _ _) = [0, 0]
        mask _           = []

idFor :: OilrProg -> String
idFor (PRO s:_) = s
idFor (RUL s:_) = s
idFor is = error $ "Malformed rule or procedure body: " ++ show is

slotsFor :: OilrProps -> String -> String
slotsFor oilr id = intercalate "," $ take n $ repeat "NULL"
    where
        n = definiteLookup id $ nSlots oilr

maskFor :: OilrProps -> String -> String
maskFor oilr id = intercalate "," $ map show $ definiteLookup id $ stateMask oilr

-- Generate C prototypes so that the ordering of definitions
-- doesn't matter
prototypes :: [OilrProg] -> [String]
prototypes decls = map proto decls
    where
        proto ((PRO "Main"):_) = ""
        proto ((PRO s):_) = "\nvoid " ++ s ++ "();"
        proto ((RUL s):_) = "\nvoid " ++ s ++ "(long recursive, DList **state);"
        proto _ = error "Found an ill-formed definition"


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


makeSearchSpaces :: Int -> OilrIndexBits -> OilrProg -> Mapping Pred [Int]
makeSearchSpaces cap bits is = map (sigsForPred cap bits) $ trace (show preds) preds
    where
        preds = ( nub . extractPredicates ) is


compileDefn :: OilrProps -> OilrProg -> [String]
compileDefn oilr is@(PRO _:_) = compileProc oilr is 
compileDefn oilr is@(RUL _:_) = compileRule oilr is
compileDefn _ (i:is)  = error $ "Found a definition starting with " ++ show i


compileProc :: OilrProps -> OilrProg -> [String]
compileProc oilr is = map compile is
    where
        compile (PRO "Main") = compile (PRO "_GPMAIN")
        compile (PRO id) = intercalate "\n" [ redef "ABORT" "return"
                                            , redef "RECURSE" "" 
                                            , startCFunction id [] ]
        compile (CAL id) = makeCFunctionCall "CALL" [id, slotsFor oilr id]
        compile (ALP id) =
            makeCFunctionCall "ALAP" [id, useRecursion oilr, slotsFor oilr id]
        compile END      = endCFunction
        compile i  =  error $ "Unexpected instruction: " ++ show i

redef :: String -> String -> String
redef macro code = intercalate "\n" [ "#undef " ++  macro
                                    , concat [ "#define ", macro, " ", code ] ]

unbindAndRet :: OilrProps -> String -> String
unbindAndRet oilr id =
    concat [ "do {"
           , makeCFunctionCall "trace" ["boolFlag?'s':'f'"]
           , makeCFunctionCall "unbindAll" ["matches", show $ definiteLookup id $ nSlots oilr]
           , "return;"
           , "} while (0);" ]

recursionCode :: String -> String
recursionCode id = makeCFunctionCall id [ "recursive<<1", "state" ]


{-         defines rec id = "\n#undef ABORT\n#undef RECURSE"
            ++ "\n#define ABORT "
            ++ ( if nSlots > 0
                  then "do { trace(boolFlag?'s':'f'); unbindAll(matches, " ++ show nSlots ++ "); return ; } while (0)\n"
                  else "return" )
            ++ ( "\n#define RECURSE " ++ if rec then id ++ "(1, state)" else "" )
        preamble id = "\n\tElement *matches[] = { NULL, " ++ slots ++ "};"
              ++ "\n\tDList   *state[]   = {" ++ slots ++ "};\n"
              ++ "#ifdef OILR_EXECUTION_TRACE\n\t oilrCurrentRule=" ++ show id ++ ";\n#endif\n" -}

useRecursion :: OilrProps -> String
useRecursion oilr = if RecursiveRules `elem` flags oilr then "1" else "0"
    

compileRule :: OilrProps -> OilrProg -> [String]
compileRule oilr is = map compile is
    where
        compile (RUL id)    = intercalate "\n" $ ruleHeader oilr id
        compile ORF         = orFail
        compile (ORB n)     = orBack n
        compile (ADN n)     = addBoundNode n
        compile (ADE e s t) = addBoundEdge e s t
        compile (RTN n)     = setBoundRoot n
        compile (URN n)     = unsetBoundRoot n
        compile (DEN n)     = deleteBoundNode n
        compile (DEE e)     = deleteBoundEdge e
        compile UBA         = exitRule
        compile END         = endCFunction
        compile i           = compileInstr (spaces oilr) i
{-        compile (LUN n sig)      = [ labelFor n ++ ":"  ]
        compile (LUE n src tgt)  = [ ]
        compile (XOE src e tgt)  = [ ]
        compile (XIE src e tgt)  = [ ]
        compile (NEC src tgt)    = [ ] -}


ruleHeader :: OilrProps -> String -> [String]
ruleHeader oilr id = [ redef "ABORT" $ unbindAndRet oilr id
                     , redef "RECURSE" $ recursionCode id
                     , startCFunction id [("long", "recursive")
                                         ,("DList", "**state")]
                     , concat ["\tElement *matches[] = { NULL, " , slots, "};"]
                     , intercalate "\n" [ "#ifdef OILR_EXECUTION_TRACE"
                                        , "\toilrCurrentRule=" ++ show id ++ ";"
                                        , "#endif" ]
                     , concat [ "\tlong stateMask[] = { ", mask, "};"]
                     , concat [ "\tlong i; for (i=0; i<"
                              , show maskSize
                              , "; i++) state[i] = (DList*) ((long) state[i] & stateMask[i]);" ]
                     , "" ]
    where
        slots = slotsFor oilr id
        mask  = maskFor  oilr id
        maskSize = length $ definiteLookup id $ stateMask oilr



orFail :: String
orFail = concat [ "\tif (!boolFlag) " , exitRule , ";"]

orBack :: Int -> String
orBack n = concat [ "\tif (!boolFlag) goto " , labelFor n , ";"]

addBoundNode :: Int -> String
addBoundNode n = modifyAndBind n "addNode" []

addBoundEdge :: Int -> Int -> Int -> String
addBoundEdge e s t = if s==t
                        then modifyAndBind e "addLoop" $ [ bindingFor s ]
                        else modifyAndBind e "addEdge" $ map bindingFor [s, t]

deleteBoundNode :: Int -> String
deleteBoundNode n = makeCFunctionCall "deleteNode" [ bindingFor n ]

deleteBoundEdge :: Int -> String
deleteBoundEdge e = makeCFunctionCall "deleteEdge" [ bindingFor e ]

setBoundRoot :: Int -> String
setBoundRoot n = makeCFunctionCall "setRoot" [bindingFor n]

unsetBoundRoot :: Int -> String
unsetBoundRoot n = makeCFunctionCall "unsetRoot" [bindingFor n]

bindingFor :: Int -> String
bindingFor n = concat [ "matches[" , show n , "]" ]

exitRule :: String
exitRule = "\tABORT;"

modifyAndBind :: Int -> String -> [String] -> String
modifyAndBind i fun args = concat [ '\t' : bindingFor i , " = ", makeCFunctionCall fun args ]

{- compileDefn :: Bool -> Mapping Pred [Int] -> OilrProg -> String
compileDefn recurse idx is = case head is of
    -- PRO _      -> concat (defines False "":cInstrs)
    -- RUL r      -> concat $ defines recurse r : (head cInstrs) : preamble r : (tail cInstrs)
    -- _          -> error "Definition doesn't begin with PRO or RUL instruction"
    where
        cInstrs = map (compileInstr idx) is
        defines rec id = "\n#undef ABORT\n#undef RECURSE"
            ++ "\n#define ABORT "
            ++ ( if nSlots > 0
                  then "do { trace(boolFlag?'s':'f'); unbindAll(matches, " ++ show nSlots ++ "); return ; } while (0)\n"
                  else "return" )
            ++ ( "\n#define RECURSE " ++ if rec then id ++ "(1, state)" else "" )
        preamble id = "\n\tElement *matches[] = { NULL, " ++ slots ++ "};"
              ++ "\n\tDList   *state[]   = {" ++ slots ++ "};\n"
              ++ "#ifdef OILR_EXECUTION_TRACE\n\t oilrCurrentRule=" ++ show id ++ ";\n#endif\n"
        slots  = concat $ intersperse "," $ take nSlots $ repeat "NULL"
        nSlots = sum $ map countMatches is
        countMatches (LUN _ _)   = 1
        countMatches (LUE _ _ _) = 1as a sole-trader.
        countMatches (LBE _ _ _) = 1
        countMatches (XOE _ _ _) = 2
        countMatches (XIE _ _ _) = 2
        countMatches (ADN _)     = 1
        countMatches (ADE _ _ _) = 1
        countMatches _           = 0
-}

makeModifyAndBind :: Int -> String -> [Int] -> String
makeModifyAndBind i fun args = "\t" ++ matchInd i ++ " = " ++ makeCFunctionCall fun (map matchInd args)
    where
        matchInd i = "matches[" ++ show i ++ "]"


compileInstr :: Mapping Pred [Int] -> Instr Int Int -> String
compileInstr _ (ADN n)         = makeModifyAndBind n "addNode" []
compileInstr _ (ADE e src tgt) | src==tgt  = makeModifyAndBind e "addLoop" [src]
                               | otherwise = makeModifyAndBind e "addEdge" [src, tgt]

compileInstr _ OK            = "\tif (recursive) { RECURSE; boolFlag=1; }\n"
compileInstr _ RET           = "return;"

compileInstr _ (PRO "Main")  = startCFunction "_GPMAIN" []
compileInstr _ (PRO s)       = startCFunction s []
compileInstr _ (RUL s)       = startCFunction s []
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
makeCFunction name lines = concat [startCFunction name [],  body, "\n}\n"]
    where
        body = intercalate "\n\t" lines

startCFunction :: String -> [(String, String)] -> String
startCFunction name args = concat [ "void " , name, "(" , argString , ") {\n"
                                  , "\tdebug(\"Entering ", name, "()\\n\");" ]
    where
        argString = intercalate ", " [ t ++ " " ++ v | (t,v) <- args ]

endCFunction :: String
endCFunction = "}\n"

asLongAsPossible :: String -> [Int] -> String
asLongAsPossible fname args = concat [ "\tdo {\n\toilrReport();\n\t", makeCFunctionCallIntArgs fname args, "\t} while (boolFlag);\n\tboolFlag=1;\n" ]

makeCFunctionCallIntArgs :: String -> [Int] -> String
makeCFunctionCallIntArgs fname args = makeCFunctionCall fname $ map show args

makeCFunctionCall :: String -> [String] -> String
makeCFunctionCall fname args = '\t':concat [ fname , "(", argStr, ");" ]
    where
        argStr = intercalate ", " args

labelFor :: Int -> String
labelFor n = "trav_no_" ++ show n
 

