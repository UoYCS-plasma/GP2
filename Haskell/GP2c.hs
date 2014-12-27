module Main where

import System.IO
import System.Environment
import System.Console.GetOpt

import ParseProgram
import ParseGraph
import ParseLib
import OilrMachine.Instructions
import OilrMachine.Compile
import OilrMachine.HostCompile
import OilrMachine.NullBackend
import OilrMachine.CBackend


compiler = "cc"

{- options :: [ OptDescr Flag ]
options = [ Option ['c'] ["one"] (NoArg $ MaxGraphs 1) "output a single graph, instead of all possible graphs",
            Option ['n'] ["no-iso"] (OptArg maxIso "MAX") "disable the isomorphism checker, limiting to a maximum of MAX result graphs" ] -}

getStem :: String -> String
getStem = takeWhile (/= '.')

main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case getOpt Permute [] args of
        (flags, [progFile], []) ->
            do
                p <- readFile progFile
                let stem = getStem progFile
                let targ = stem ++ ".c"
                putStrLn $ "Parsing " ++ progFile
                let prog = parse program p
                -- putStrLn $ show prog
                -- putStrLn ""
                putStrLn $ "Compiling " ++ progFile ++ " to " ++ targ
                let code = cCompile $ compileGPProg prog
                writeFile targ code
                return ()
        (flags, [progFile, hostFile], []) ->
            do
                putStrLn $ " ** Warning: host-graph burned into executable!"
                p <- readFile progFile
                h <- readFile hostFile
                let stem = getStem progFile
                let targ = stem ++ ".c"
                putStrLn $ "Parsing " ++ progFile
                let prog = parse program p
                -- putStrLn $ show prog
                -- putStrLn ""
                putStrLn $ "Parsing " ++ hostFile
                let host = parse hostGraph h
                putStrLn $ "Compiling " ++ progFile ++ " to " ++ targ
                let code = cCompile $ compileHostGraph host ++ compileGPProg prog

                writeFile targ code
                return ()


