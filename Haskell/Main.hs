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
import Graph (dumpGraphViz)

notImplemented = error "whatever"

loadProgram :: String -> IO String
loadProgram = readFile

loadGraph :: String -> IO String
loadGraph = readFile


-- TODO: only gets the first rule declaration
extractRuleDecl :: GPProgram -> AstRule
extractRuleDecl (Program ((RuleDecl r):xs)) = r
extractRuleDecl (Program (_:xs)) = extractRuleDecl (Program xs)

report :: String -> IO ()
report s = do
    putStrLn s
    putStrLn ""


-- TODO: not doing root-node matching in GraphMatch.hs!
-- TODO: returned graphs are incorrect after rule application

myMain = do
    [prog, graph] <- getArgs
    p <- loadProgram prog
    g <- loadGraph graph
    let hg = makeHostGraph $ parse hostGraph g
    let rd = makeRule  ( extractRuleDecl $ parse program p ) "" empty
    report $ dumpGraphViz hg
    --putStrLn $ concatMap prettyPrint $ applyRule hg rd
    report $ concatMap dumpGraphViz $ applyRule hg rd
    --putStrLn $ show $ applyRule hg rd
