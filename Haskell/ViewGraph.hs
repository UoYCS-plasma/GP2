module Main where

import System.Cmd
import System.Exit
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Posix.Temp

import ParseLib
import ParseGraph
import ProcessAst
import GPSyntax
import GraphViz


usage :: String
usage = "Usage: ViewGraph <GP_graph>"


main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute [] args of
        (flags, [gr1file], []) -> do
            g1 <- readFile gr1file
            let hg1 = makeHostGraph $ parse hostGraph g1
            (tmp, handle) <- mkstemp "/tmp/gp2graph_"
            hPutStr handle $ drawGraph hg1
            hFlush handle
            let outfile = tmp ++ ".pdf"
            let dotCmd = "dot -Tpdf -o" ++ outfile ++ " " ++ tmp
            putStrLn outfile
            putStrLn dotCmd
            s1 <- system dotCmd
            s2 <- system $ "evince " ++ outfile
            case (s1, s2) of
                (ExitSuccess, ExitSuccess) -> putStrLn "success"
                (ExitSuccess, _) -> putStrLn "Loading Evince PDF viewer failed"
                (_, _) -> putStrLn "Running dot command failed"
            hClose handle
        (_, _, errs) -> do
            error (concat errs ++ usage)

