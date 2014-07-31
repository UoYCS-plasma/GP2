module Main where

import System.IO
import System.Environment
import System.Console.GetOpt

import ParseLib
import ParseGraph
import ProcessAst
import GPSyntax


usage :: String
usage = "Usage: ViewGraph <GP_graph>"


main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute [] args of
        (flags, [gr1file], []) -> do
            g1 <- readFile gr1file
            let hg1 = makeHostGraph $ parse hostGraph g1
            
        (_, _, errs) -> do
            error (concat errs ++ usage)

