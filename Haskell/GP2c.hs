module Main where

import System.IO
import System.Environment
import System.Console.GetOpt
import System.Process (system)
import System.Exit

import Text.Parsec

import OILR3.Instructions
import OILR3.HostCompile
import OILR3.ProgCompile
import OILR3.CBackend
import OILR3.CRuntime

-- import GPSyntax -- debug code
import ParseGraph
import ParseProgram
import ProcessAst (makeHostGraph)

debugCompiler = "gcc -g -Wall -Wno-error=unused-label -Wno-unused-label -Werror -o"
perfCompiler  = "gcc -O2 -fomit-frame-pointer -Wall -Wno-error=unused-label -Wno-unused-label -Werror -o"


options :: [ OptDescr Flag ]
options = [ Option ['o'] ["no-oilr"] (NoArg $ DisableOilr) "Use only a single OILR index for all nodes.",
            Option ['n'] ["no-search-plan"] (NoArg $ DisableSearchPlan) "Disable the search plan; use brute-force nodes-then-edges strategy",
            Option ['d'] ["debug"]   (NoArg $ EnableDebugging) "Enable verbose debugging output on compiled program's stdout" ]

getStem :: String -> String
getStem = takeWhile (/= '.')

parseHostGraph graphFile = do
    g <- readFile graphFile
    case parse hostGraph graphFile g of
        Left e     -> error "Compilation of host graph failed" -- print e
        Right host -> return $ makeHostGraph host

parseProgram progFile = do
    p <- readFile progFile
    case parse program progFile p of
        Left e     -> error "Compilation of program failed"
        Right prog -> return prog

callCCompiler cc obj cFile = do
    -- TODO: use of system is ugly and potentially dangerous!
    exStatus <- system (cc ++ " " ++ obj ++ " " ++ cFile)
    case exStatus of
        ExitSuccess -> return ()
        (ExitFailure _) -> error "Compilation failed."



main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case getOpt Permute options args of
        (flags, [progFile, hostFile], []) -> do
            let stem = getStem progFile
            let targ = stem ++ ".c"
            let exe  = stem
            -- p <- readFile progFile
            pAST <- parseProgram progFile
            hAST <- parseHostGraph hostFile
            let prog = compileProgram flags pAST
            let host = compileHostGraph hAST
            -- putStrLn $ show prog
            let progC = progToC flags prog
            let hostC = hostToC host
            let compiler = if EnableDebugging `elem` flags then debugCompiler else perfCompiler
            writeFile targ $ progC ++ hostC
            callCCompiler compiler exe targ
        _ -> do
            error "Nope"

