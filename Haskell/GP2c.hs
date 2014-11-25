module Main where

import System.IO
import System.Environment
import System.Console.GetOpt

import ParseProgram
import ParseLib
import Cassava.Instructions
import Cassava.Compile
import Cassava.NullBackend
import Cassava.CBackend


emitInstrs prog = do
    let asm = cCompile prog
    putStrLn asm

main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case getOpt Permute [] args of
        (flags, [progFile], []) ->
            do
                p <- readFile progFile
                let prog = parse program p
                -- putStrLn $ show prog
                -- putStrLn ""
                emitInstrs $ compileGPProg prog
                return ()


