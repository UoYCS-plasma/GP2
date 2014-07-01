module Main where

import System.IO
import System.Environment

import Interp
import ParseProgram
import ParseLib
import ParseGraph
import ProcessAst
import GPSyntax
import ExAr

notImplemented = error "whatever"

loadProgram :: String -> IO String
loadProgram = readFile

loadGraph :: String -> IO String
loadGraph = readFile

extractRuleDecl :: GPProgram -> AstRule
extractRuleDecl (Program ((RuleDecl r):xs)) = r
extractRuleDecl (Program (_:xs)) = extractRuleDecl (Program xs)



main :: IO ()
main = do
    [prog, graph] <- getArgs
    p <- loadProgram prog
    g <- loadGraph graph
    putStrLn $ show $ parse hostGraph g
    putStrLn ""
    let rd = extractRuleDecl $ parse program p
    putStrLn $ show $ makeRule rd "" empty
