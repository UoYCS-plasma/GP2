module Main where

import System.IO
import System.Environment

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

printGraph :: HostGraph -> String
printGraph = graphToGP2 . makePrintableGraph

printResult :: FilePath -> Result -> IO ()
printResult fileName (gs, fc, uc) = do
   putStrLn $ show fc ++ " fails."
   putStrLn $ show uc ++ " unfinished computations."
   printGraphData fileName 1 gs

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
main = do
    [progFile, graphFile, max] <- getArgs
    p <- readFile progFile
    g <- readFile graphFile
    let maxRules = read max
    let host = makeHostGraph $ parse hostGraph g
    let (prog, syms) = makeGPProgram $ parse program p
    putStrLn $ "GP 2 program " ++ progFile ++ " executed on host graph in " ++ graphFile ++ "."
    putStrLn $ "Program execution stopped at " ++ show maxRules ++ " rule applications.\n"
    let result = runProgram prog maxRules host 
    printResult progFile result

