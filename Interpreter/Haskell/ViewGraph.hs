module Main where

import System.Cmd
import System.Exit
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Posix.Temp
import Text.Parsec
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
            case parse hostGraph gr1file g1 of
                Left  e -> print e
                Right p -> do
                    let hg1 = makeHostGraph p
                    (tmp, handle) <- mkstemp "/tmp/gp2graph_"
                    hPutStr handle $ drawGraph hg1
                    hFlush handle
                    let outfile = tmp ++ ".pdf"
                    let dotCmd = "neato -Tpdf -o" ++ outfile ++ " " ++ tmp
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

