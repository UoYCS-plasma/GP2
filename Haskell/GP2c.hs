module Main where

import System.IO
import System.Environment
import System.Console.GetOpt
import System.Process (system)
import System.Exit

import OILR3.Instructions
import OILR3.CRuntime
-- import OILR3.CCompile
import GPSyntax -- debug code

compiler = "gcc -g -O2 -Wall -Werror -o"

{- options :: [ OptDescr Flag ]
options = [ Option ['c'] ["one"] (NoArg $ MaxGraphs 1) "output a single graph, instead of all possible graphs",
            Option ['n'] ["no-iso"] (OptArg maxIso "MAX") "disable the isomorphism checker, limiting to a maximum of MAX result graphs" ] -}

getStem :: String -> String
getStem = takeWhile (/= '.')



callCCompiler cc obj cFile = do
    exStatus <- system (cc ++ " " ++ obj ++ " " ++ cFile)
    case exStatus of
        ExitSuccess -> return ()
        (ExitFailure _) -> error "Compilation failed."

main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case getOpt Permute [] args of
        (flags, [progFile], []) -> do
            let stem = getStem progFile
            let targ = stem ++ ".c"
            let exe  = stem
            writeFile targ cRuntime
            callCCompiler compiler exe targ
        _ -> do
            error "Nope"

{-
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case getOpt Permute [] args of
        (flags, [progFile], []) ->
            do
                p <- readFile progFile
                let stem = getStem progFile
                let targ = stem ++ ".c"
                putStrLn $ "Parsing " ++ progFile
                case parse program progFile p of
                  Left  err  -> print err
                  Right prog -> do
                    putStrLn $ "Compiling " ++ progFile ++ " to " ++ targ
                    -- putStrLn $ show prog
                    -- _ <- mapM putStrLn $ map show $ map characteriseRule $ extractDecls prog
                    putStrLn ""
                    -- let code = cCompile $ compileGPProg prog
                    -- writeFile targ code
        (flags, [progFile, hostFile], []) ->
            do
                putStrLn $ " ** Warning: host-graph burned into executable!"
                p <- readFile progFile
                h <- readFile hostFile
                let stem = getStem progFile
                let targ = stem ++ ".c"
                putStrLn $ "Parsing " ++ progFile
                case parse program progFile p of
                  Left  err  -> print err
                  Right prog -> do
                    putStrLn $ "Parsing " ++ hostFile
                    case parse hostGraph hostFile h of
                      Left  err  -> print err
                      Right host -> do
                        putStrLn $ "Compiling " ++ progFile ++ " to " ++ targ
                        let code = cCompile $ compileHostGraph host ++ compileGPProg prog
                        writeFile targ code
-}
