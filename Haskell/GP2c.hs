module Main where

import System.IO
import System.Environment
import System.Console.GetOpt

import ParseProgram
import Cassava.Instructions


main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case getOpt Permute options args of
        (flags, [progFile], []) ->
            do
                p <- readFile progFile

