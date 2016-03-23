module Main where

import System.IO
import System.Environment
import System.Console.GetOpt
import System.Process (system)
import System.Exit

import Text.Parsec
import Data.List

import OILR4.Instructions
import OILR4.HostCompile
import OILR4.Config
import OILR4.IR
import OILR4.Optimiser
import OILR4.OILROptim
import OILR4.X86Backend
import OILR4.CBackend


-- import GPSyntax -- debug code
import ParseGraph
import ParseProgram
import ProcessAst (makeHostGraph)


debugCompiler = "gcc -g "
perfCompiler  = "gcc -O2 "

compilerFlagsCommon = "-Wno-format -Wno-unused-label -Wall -Wextra -m32 -o "

getCompilerFor flags = concat [ cc, compilerFlagsCommon ]
    where
        cc = if ( EnableDebugging `elem` flags || EnableParanoidDebugging `elem` flags)
                then debugCompiler
                else perfCompiler

options :: [ OptDescr Flag ]
options = [ Option ['o'] ["no-oilr"] (NoArg $ DisableOilr) "Use only a single OILR index for all nodes.",
            Option ['S'] ["dump-prog"] (NoArg $ OilrInstructions) "Emit raw OILR instructions instead of compiling via C",
            Option ['r'] ["recursive"] (NoArg $ RecursiveRules) "Execute looped rules recursively.",
            Option ['n'] ["no-search-plan"] (NoArg $ DisableSearchPlan) "Disable the search plan; use brute-force nodes-then-edges strategy",
            Option ['T'] ["trace"]   (NoArg $ EnableExecutionTrace) "Enable execution trace via GraphViz" ,
            Option ['c'] ["compact-lists"]   (NoArg $ CompactLists) "Use compact but non-portable doubly-linked list representation" ,
            Option ['d'] ["debug"]   (NoArg $ EnableDebugging) "Enable verbose debugging output on compiled program's stderr" ,
            Option ['3'] ["32-bit"]  (NoArg $ Compile32Bit) "Compile a 32-bit executable" ,
            Option ['D'] ["extra-debug"]   (NoArg $ EnableParanoidDebugging) "Enable paranoid graph structure checks (implies -d)" ]

getStem :: String -> String
getStem = takeWhile (/= '.')

parseHostGraph graphFile = do
    g <- readFile graphFile
    case parse hostGraph graphFile g of
        Left e     -> error $ "Compilation of host graph failed" ++ show e
        Right host -> return $ makeHostGraph host

parseProgram progFile = do
    p <- readFile progFile
    case parse program progFile p of
        Left e     -> error $ "Compilation of program failed:\n" ++ show e
        Right prog -> return prog

callCCompiler cc obj cFile = do
    -- TODO: use of system is ugly and potentially dangerous!
    exStatus <- system $ intercalate " " [cc, obj, cFile]
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
            let ir = makeIR pAST
            let cf = configureOilrMachine flags ir
            let (cf', prog) = compileProg cf $ optimise cf ir
            let c = compileC cf' prog
            -- let host = compileHostGraph hAST
            -- putStrLn $ show prog
            let compiler = getCompilerFor flags
            if OilrInstructions `elem` flags
                then putStrLn $ show prog
                else return ()
            writeFile targ $ c
            putStrLn $ intercalate " " [compiler,exe,targ]
            callCCompiler compiler exe targ
                     -- return ()
        _ -> do
            error "Nope"

