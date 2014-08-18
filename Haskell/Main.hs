module Main where

import System.IO
import System.Environment
import System.Console.GetOpt

import ApplyRule
import ExAr
import Graph (graphToGP2)
import GraphIsomorphism
import GPSyntax
import ParseGraph
import ParseLib
import ParseProgram
import PrintGraph
import ProcessAst
import RunProgram

-- Optional: For testing
import GraphMatch
import Graph
import ParseRule

import Debug.Trace

testProg = fst $ makeGPProgram $ parse program programStr

programStr = concat ["Main = r1\nr1 (m:int)\n",
                     "[ (n1 (R), m # grey) | ]\n=>\n",
                     "[ (n1, + m 1 # grey) | ]\n",
                     "interface = {n1}",
                     "where i < 5"]

testRule = makeRule (parse rule ruleStr) "Global" [("m", Symbol (Var_S IntVar False) "Global" "r1"), ("n", Symbol (Var_S IntVar False) "Global" "r1"), ("p", Symbol (Var_S IntVar False) "Global" "r1"), ("q", Symbol (Var_S IntVar False) "Global" "r1") ]


ruleStr = concat ["r1 (m,n,p,q:int)\n",
                  "[ (n1, m:n) (n2, n) (n3, p) (n4, q) |\n",
                  "(e1, n2, n3, 0) (e2, n2, n4, 1) (e3, n3, n4, 2)]\n=>\n",
                  "[ (n1, + n 1) (n2, + n 1) (n3, + n 1) (n4, p) (n5, 0) (n6, q) (n7, m:n)|\n",
                  "(e1, n1, n2, 0) (e2, n1, n3, 1) (e3, n2, n3, 2)\n",
                  "(e4, n2, n4, 0) (e5, n2, n5, 1) (e6, n4, n5, 2)\n",
                  "(e7, n3, n5, 0) (e8, n3, n6, 1) (e9, n5, n6, 2)]",
                  "interface = {n1, n2, n3, n7}"]

testlhs = fst $ makeRuleGraph (parse ruleGraph lhsStr) "Global" "r1" [("m", Symbol (Var_S IntVar False) "Global" "r1"), ("n", Symbol (Var_S IntVar False) "Global" "r1"), ("p", Symbol (Var_S IntVar False) "Global" "r1"), ("q", Symbol (Var_S IntVar False) "Global" "r1") ]

lhsStr = "[(n1, m:n) (n2, n) (n3, p) (n4, q) | (e1, n2, n3, 0) (e2, n2, n4, 1) (e3, n3, n4, 2)]" 

testHG1 = makeHostGraph $ parse hostGraph hostStr1
testHG2 = makeHostGraph $ parse hostGraph hostStr2

hostStr1 = concat ["[ (n1, 2) (n2, 2) (n3, 2) (n4, 0) (n5, 0) (n6, 0) (n7, 1:1)|\n",
                  "(e1, n1, n2, 0) (e2, n1, n3, 1) (e3, n2, n3, 2)\n",
                  "(e4, n2, n4, 0) (e5, n2, n5, 1) (e6, n4, n5, 2)\n",
                  "(e7, n3, n5, 0) (e8, n3, n6, 1) (e9, n5, n6, 2)]"]
                 
hostStr2 = concat ["[ (n1, 1) (n2, 0) (n3, 0) (n4, 1:1) |\n",
                  "(e1, n1, n2, 0) (e2, n1, n3, 1) (e3, n2, n3, 2)]"]

printGraph :: HostGraph -> String
printGraph = graphToGP2 . makePrintableGraph


printResult :: FilePath -> Result -> IO ()
printResult fileName (gs, fc, uc) = do
   putStrLn $ show fc ++ " fails."
   putStrLn $ show uc ++ " unfinished computations."
   printGraphData fileName 1 gs

-- It's better if this function creates files in a new directory,
-- but I do not know how to do this.
printGraphData :: FilePath -> Int -> [GraphData] -> IO ()
printGraphData _ _ [] = putStrLn ""
printGraphData fileName k ((graph, count):gcs) = do
    let outputFile = fileName ++ show k 
    writeFile outputFile $ printGraph graph
    if count == 1 
        then putStrLn $ "1 occurrence of the graph in file " ++ outputFile
        else putStrLn $ show count ++ " occurrences of the graph in file " ++ outputFile 
    printGraphData fileName (k+1) gcs

-- TODO: convert progFile into a string of the actual program name
-- i.e. trim off the file extension.

data Flag = Single

options :: [ OptDescr Flag ]
options = [ Option ['1'] ["one"] (NoArg Single) "output a single graph, instead of all possible graphs" ]

usage = "Usage: gp2 [flags] <prog> <hostGraph> <maxDepth>\nWhere [flags] can be:"

main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case getOpt Permute options args of
        (flags, [progFile, graphFile, max], []) -> do
            p <- readFile progFile
            g <- readFile graphFile
            let progName = takeWhile (/= '.') progFile
            let maxRules = read max
            -- Debugging: print host graph parse output
            -- putStrLn $ show $ hostGraph g
            let host = makeHostGraph $ parse hostGraph g
            putStrLn $ "\nGP 2 program " ++ progFile ++ " executed on host graph in " ++ graphFile ++ ".\n"
            -- Debugging: print program parse output
            -- putStrLn $ show $ program p
            let (prog, syms) = makeGPProgram $ parse program p
            putStrLn $ "Program execution will be stopped at " ++ show maxRules ++ " rule applications.\n"
            printResult progName $ case flags of
                                        [ Single ] -> trace "single result mode" (firstSolution prog maxRules host)
                                        _          -> runProgram prog maxRules host
        (_, _, errs) -> do
            error (concat errs ++ usageInfo usage options)

