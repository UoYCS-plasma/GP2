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

printGraphData :: [(Int, HostGraph)] -> IO ()
printGraphData = putStrLn . concatMap printCount
    where printCount (1,g) = "1 occurrence of\n\n" ++ printGraph g ++ "\n\n"
          printCount (k,g) = show k ++ " occurrences of\n\n" ++ printGraph g ++ "\n\n"

main = do
    [prog, graph] <- getArgs
    p <- loadProgram prog
    g <- loadGraph graph
    let host = makeHostGraph $ parse hostGraph g
    let (prog, syms) = makeGPProgram $ parse program p
    let pHost = makePrintableGraph host
    putStrLn $ printGraph host
    putStrLn "\n"
    let graphData = isomorphismCount $ runProgram prog host horizon
    printGraphData graphData

