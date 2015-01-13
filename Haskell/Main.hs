module Main where

import System.IO
import System.Environment
import System.Console.GetOpt
import GPSyntax
import ParseGraph
import ParseLib
import ParseProgram
import PrintGraph 
import ProcessAst
import RunProgram
import Debug.Trace

printResult :: FilePath -> Result -> IO ()
printResult fileName (gs, fc, uc, bds) = do
   putStrLn $ show fc ++ " fails."
   putStrLn $ show uc ++ " unfinished computations."
   if length gs > 0 then putStrLn $ "Rule application bounds (low, high): " ++ show bds else putStrLn ""
   printGraphData fileName 1 gs

-- It's better if this function creates files in a new directory,
-- but I do not know how to do this.
printGraphData :: FilePath -> Int -> [GraphData] -> IO ()
printGraphData _ _ [] = putStrLn ""
printGraphData fileName k ((graph, count):gcs) = do
    let outputFile = fileName ++ show k 
    writeFile outputFile $ printHostGraph graph
    if count == 1 
        then putStrLn $ "1 occurrence of the graph in file " ++ outputFile
        else putStrLn $ show count ++ " occurrences of the graph in file " ++ outputFile 
    printGraphData fileName (k+1) gcs

-- TODO: convert progFile into a string of the actual program name
-- i.e. trim off the file extension.

data Flag = MaxGraphs Int

maxIso :: Maybe String -> Flag
maxIso Nothing = MaxGraphs 1000
maxIso (Just v) = (MaxGraphs . read) v

options :: [ OptDescr Flag ]
options = [ Option ['1'] ["one"] (NoArg $ MaxGraphs 1) "output a single graph, instead of all possible graphs",
            Option ['n'] ["no-iso"] (OptArg maxIso "MAX") "disable the isomorphism checker, limiting to a maximum of MAX result graphs" ]

usage = "Usage: gp2 [flags] <prog> <hostGraph> <maxDepth>\nWhere [flags] can be:"

main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case getOpt Permute options args of
        (flags, [progFile, graphFile, max], []) -> do
            p <- readFile progFile
            g <- readFile graphFile
            let progName = takeWhile (/= '.') progFile
            let maxRules = read max
            -- Debugging: print host graph parse output
            -- putStrLn $ show $ hostGraph g
            let host = makeHostGraph $ parse hostGraph g
            putStrLn $ "\nGP 2 program " ++ progFile ++ " executed on host graph in " ++ graphFile ++ ".\n"
            -- Debugging: print program parse output
            -- putStrLn $ show $ program p
            let (prog, syms) = makeGPProgram $ parse program p
            putStrLn $ "Program execution will be stopped at " ++ show maxRules ++ " rule applications.\n"
            printResult progName $ case flags of
                                        [ MaxGraphs n ] -> trace (show n ++ " result mode") (nSolutions n prog maxRules host)
                                        _          -> runProgram prog maxRules host
        (_, _, errs) -> do
            error (concat errs ++ usageInfo usage options)

