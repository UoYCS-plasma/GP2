module Main where

import System.IO
import System.Environment

import ApplyRule
import ParseProgram
import ParseLib
import ParseGraph
import ProcessAst
import GPSyntax
import ExAr
import Graph (dumpGraphViz)
import GraphIso
import RunProgram

loadProgram :: String -> IO String
loadProgram = readFile

loadGraph :: String -> IO String
loadGraph = readFile

horizon = 3

printGraph :: HostGraph -> IO ()
printGraph g = do
    putStrLn $ dumpGraphViz g
    putStrLn ""

printGraphData :: [(Int, HostGraph)] -> IO ()
printGraphData = putStrLn . concatMap printGraph
    where printGraph (1,g) = "1 occurrence of\n" ++ dumpGraphViz g
          printGraph (k,g) = show k ++ " occurrences of\n" ++ dumpGraphViz g

main = do
    [prog, graph] <- getArgs
    p <- loadProgram prog
    g <- loadGraph graph
    let host = makeHostGraph $ parse hostGraph g
    let (prog, syms) = makeGPProgram $ parse program p
    printGraph host
    let graphData = isomorphismCount $ runProgram prog host horizon
    printGraphData graphData

