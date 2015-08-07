module OILR3.CBackend (hostToC, progToC) where

import OILR3.Instructions
import OILR3.CRuntime

import Data.List
import Debug.Trace

type OilrProg = [Instr Int Int]

hostToC :: OilrProg -> String
hostToC is = makeCFunction "_HOST" $ map compileInstr is


progToC :: [OilrProg] -> String
progToC iss = consts ++ cRuntime ++ predeclarations iss ++ concat defns
    where
        defns = map compileDefn iss
        consts = "#define OILR_INDEX_SIZE (1<<" ++ (show $ deduceOilrIndexSize iss) ++ ")\n"

-- Generate C declarations so that the ordering of definitions
-- doesn't matter
predeclarations :: [OilrProg] -> String
predeclarations iss = concatMap declare iss
    where
        declare ((DEF "Main"):_) = ""
        declare ((DEF s):_) = "\nvoid " ++ s ++ "();"
        declare _ = error "Found an ill-formed definition"

{- makeSearchSpacesForDecl is = concatMap sSpc is
    where
        sSpc (LUN n sig) = matchingSpaces sig
        sSpc _           = []
-}
deduceOilrIndexSize iss = bits o + bits i + bits l + bits r
    where
        (o, i, l, r) = foldr mergePreds (0,0,0,0) $ concatMap harvestPreds $ concat iss
        harvestPreds (LUN _ p) = [p]
        harvestPreds _         = []
        mergePreds (o',i',l',r') (o,i,l,r) =
            (max o $ extract o', max i $ extract i', max l $ extract l', max r $ extract r')
        extract (Equ n) = n
        extract (GtE n) = n
        bits n = head $ dropWhile (\x -> 2^x < n) [0,1..]



compileDefn :: OilrProg -> String
compileDefn is = concatMap compileInstr is


compileInstr :: Instr Int Int -> String
compileInstr (ADN n)         = makeCFunctionCall "addNode" [n]
compileInstr (ADE e src tgt) = makeCFunctionCall "addEdgeById" [src, tgt]
compileInstr (RTN n)         = makeCFunctionCall "setRoot" [n]
compileInstr (DEN n)         = makeCFunctionCall "deleteNode" [n]
compileInstr (DEE e)         = makeCFunctionCall "deleteEdge" [e]

compileInstr RET           = "return;"
compileInstr (CAL s)       = makeCFunctionCall s []
compileInstr (ALP s)       = asLongAsPossible s []

compileInstr (DEF "Main")  = startCFunction "_GPMAIN"
compileInstr (DEF s)       = startCFunction s
compileInstr END           = endCFunction 

compileInstr (LUN n sig)     = makeCFunctionCall "findNode" [n]
compileInstr (LUE n src tgt) = makeCFunctionCall "findEdge" [n, src, tgt]

makeCFunction :: String -> [String] -> String
makeCFunction name lines = concat [startCFunction name,  body, "\n}\n"]
    where
        body = intercalate "\n\t" lines

startCFunction :: String -> String
startCFunction name = concat [ "\nvoid ", name, "() {\n\t" ]

endCFunction :: String
endCFunction = "}\n"

asLongAsPossible :: String -> [Int] -> String
asLongAsPossible fname args = concat [ "do {\n", makeCFunctionCall fname args, "} while (success);\n" ]


makeCFunctionCall :: String -> [Int] -> String
makeCFunctionCall fname args = concat [ fname , "(", argStr, ");\n" ]
    where
        argStr = intercalate ", " $ map show args



