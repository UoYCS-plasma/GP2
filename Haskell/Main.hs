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

{-- Optional: For testing
import GraphMatch
import Graph
import ParseRule

import Debug.Trace-}

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
                                        [ Single ] -> {- trace "single result mode" -} (nSolutions 1 prog maxRules host)
                                        _          -> runProgram prog maxRules host
        (_, _, errs) -> do
            error (concat errs ++ usageInfo usage options)

