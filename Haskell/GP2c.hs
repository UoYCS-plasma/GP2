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

debugCompiler = "gcc -g "
perfCompiler  = "gcc -O2 -fomit-frame-pointer "

compilerFlags32 = " -m32 -Wno-error=format= "
compilerFlags64 = " -m64 "

compilerFlagsCommon = " -Wall -Wno-error=unused-label -Wno-unused-label -Wno-error=unused-variable -Werror -o"

getCompilerFor flags = concat [ cc, arch, compilerFlagsCommon ]
    where
        cc = if ( EnableDebugging `elem` flags || EnableParanoidDebugging `elem` flags)
                then debugCompiler
                else perfCompiler
        arch = if Compile32Bit `elem` flags
                    then compilerFlags32
                    else compilerFlags64


options :: [ OptDescr Flag ]
options = [ Option ['o'] ["no-oilr"] (NoArg $ DisableOilr) "Use only a single OILR index for all nodes.",
            Option ['S'] ["dump-prog"] (NoArg $ OilrInstructions) "Emit raw OILR instructions instead of compiling via C",
            Option ['r'] ["recursive"] (NoArg $ RecursiveRules) "Execute looped rules recursively.",
            Option ['n'] ["no-search-plan"] (NoArg $ DisableSearchPlan) "Disable the search plan; use brute-force nodes-then-edges strategy",
            Option ['T'] ["trace"]   (NoArg $ EnableExecutionTrace) "Enable execution trace via GraphViz" ,
            Option ['d'] ["debug"]   (NoArg $ EnableDebugging) "Enable verbose debugging output on compiled program's stderr" ,
            Option ['3'] ["32-bit"]  (NoArg $ Compile32Bit) "Compile a 32-bit executable" ,
            Option ['D'] ["extra-debug"]   (NoArg $ EnableParanoidDebugging) "Enable paranoid graph structure checks (implies -d)" ]

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
            if OilrInstructions `elem` flags
                then putStrLn $ show prog
                else return ()
            let progC = progToC flags prog
            let hostC = hostToC host
            let compiler = getCompilerFor flags
            writeFile targ $ progC ++ hostC
            callCCompiler compiler exe targ
        _ -> do
            error "Nope"

