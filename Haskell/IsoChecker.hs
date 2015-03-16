module Main where

import System.IO
import System.Environment
import System.Console.GetOpt
import Text.Parsec
import ParseGraph
import GraphIsomorphism
import ProcessAst
-- import GPSyntax

usage :: String
usage = "Usage: IsoChecker <graph1> <graph2>"

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute [] args of
    (flags, [gr1file, gr2file], []) -> do
      g1 <- readFile gr1file
      g2 <- readFile gr2file
      case (parse hostGraph gr1file g1, parse hostGraph gr2file g2) of
        (Left e1,   Left e2)   -> do {print e1 ; print e2}
        (Left e1,   Right _)   -> print e1
        (Right _,   Left e2)   -> print e2
        (Right g1', Right g2') -> do
          let hg1 = makeHostGraph g1'
          let hg2 = makeHostGraph g2'
          putStr $ (if isomorphic hg1 hg2 then "ISOMORPHIC: " else "NON-ISOMORPHIC")
          putStrLn $ gr1file ++ " and " ++ gr2file
    (_, _, errs) -> do
      error (concat errs ++ usage)
