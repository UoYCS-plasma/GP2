module Main where

import System.IO
import System.Environment
import System.Console.GetOpt
import Debug.Trace

import ParseLib
import ParseGraph
import GraphIsomorphism
import ProcessAst
import GPSyntax

usage :: String
usage = "Usage: IsoChecker <graph1> <graph2>"


main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute [] args of
        (flags, [gr1file, gr2file], []) -> do
            g1 <- readFile gr1file
            g2 <- readFile gr2file
            let hg1 = makeHostGraph $ parse hostGraph g1
            let hg2 = makeHostGraph $ parse hostGraph g2
            let n = length $ isomorphismCount [hg1, hg2]
            case trace (show n) n of 
                1 -> putStrLn $ "\ESC[032mISOMORPHIC: \ESC[0m" ++ gr1file ++ " and " ++ gr2file
                _ -> putStrLn $ "\ESC[031mNON-ISOMORPHIC: \ESC[0m" ++ gr1file ++ " and " ++ gr2file
        (_, _, errs) -> do
            error (concat errs ++ usage)
