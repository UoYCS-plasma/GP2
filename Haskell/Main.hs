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

horizon = 3

printGraph :: HostGraph -> String
printGraph = graphToGP2 . makePrintableGraph

printGraphData :: String -> Int -> [(Int, HostGraph)] -> IO ()
printGraphData _ _ [] = putStrLn ""
printGraphData fileName k ((count,graph):xs) = do
    let outputFile = fileName ++ show k 
    writeFile outputFile $ printGraph graph
    if count == 1 
        then putStrLn $ "1 occurrence of the graph in file " ++ outputFile
        else putStrLn $ show count ++ " occurrences of the graph in file " ++ outputFile 
    printGraphData fileName (k+1) xs

main = do
    [progFile, graphFile] <- getArgs
    p <- readFile progFile
    g <- readFile graphFile
    let host = makeHostGraph $ parse hostGraph g
    let (prog, syms) = makeGPProgram $ parse program p
    let pHost = makePrintableGraph host
    putStrLn $ printGraph host ++ "\n"
    let graphData = isomorphismCount $ runProgram prog host horizon
    printGraphData progFile 1 graphData

