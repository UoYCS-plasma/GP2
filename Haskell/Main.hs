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

loadProgram :: String -> IO String
loadProgram = readFile

loadGraph :: String -> IO String
loadGraph = readFile

horizon = 3

printGraph :: HostGraph -> String
printGraph = graphToGP2 . makePrintableGraph

printGraphData :: String -> Int -> [(Int, HostGraph)] -> IO ()
printGraphData _ _ [] = putStrLn "\n"
printGraphData fileName k ((count,graph):xs) = do 
    writeFile (fileName ++ show k) $ printGraph graph
    if count == 1 
    then putStrLn $ "1 occurrence of graph in file " ++ fileName ++ show k
    else putStrLn $ show count ++ " occurrences of graph in file " ++ fileName ++ show k    
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

