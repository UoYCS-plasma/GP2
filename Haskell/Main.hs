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

report :: String -> IO ()
report s = do
    putStrLn s
    putStrLn ""

main = do
    [prog, graph] <- getArgs
    p <- loadProgram prog
    g <- loadGraph graph
    let hg = makeHostGraph $ parse hostGraph g
    let (prog, syms) = makeGPProgram $ parse program p
    report $ dumpGraphViz hg
    report $ concatMap dumpGraphViz $ runProgram prog hg horizon 
    {- let (g1:g2:gs) = runProgram prog hg horizon
    let b = isomorphic g1 g2
    if b then putStrLn "True" else putStrLn "False" -}

