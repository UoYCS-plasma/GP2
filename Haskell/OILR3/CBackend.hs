module OILR3.CBackend (hostToC, progToC) where

import OILR3.Instructions
import OILR3.CRuntime

import Data.List

hostToC :: [Instr] -> String
hostToC is = makeCFunction "_HOST" $ map compileInstr is


progToC :: [Instr] -> String
progToC is = cRuntime ++ predeclarations is ++ concatMap compileInstr is

predeclarations :: [Instr] -> String
predeclarations is = concatMap declare is
    where
        declare (DEF "Main") = ""
        declare (DEF s) = "\nvoid " ++ s ++ "();"
        declare _ = ""



-- WARNING: the C runtime expects nodes indexed from zero, while the Haskell
-- graph structure generates indices from 1
compileInstr :: Instr -> String
compileInstr ADN           = makeCFunctionCall "addNode" []
compileInstr (ADE src tgt) = makeCFunctionCall "addEdgeById" [src-1, tgt-1]
compileInstr (RTN n)       = makeCFunctionCall "setRoot" [n-1]

compileInstr RET           = "return;"
compileInstr (CAL s)       = makeCFunctionCall s []
compileInstr (ALP s)       = asLongAsPossible s []

compileInstr (DEF "Main")  = startCFunction "_GPMAIN"
compileInstr (DEF s)       = startCFunction s
compileInstr END           = endCFunction 

compileInstr (LUN sig)     = makeCFunctionCall "findNode" []

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



