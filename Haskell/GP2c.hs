module Main where

import System.IO
import System.Environment
import System.Console.GetOpt

import ParseProgram
import Cassava.Instructions
import Cassava.Compile
import Cassava.NullBackend


main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case getOpt Permute [] args of
        (flags, [progFile], []) ->
            do
                p <- readFile progFile
                return ()


