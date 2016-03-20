{-# LANGUAGE QuasiQuotes #-}
module OILR4.CRuntime where

import OILR4.EmbedFileContents

cRuntime :: String
cRuntime = [litFile|OILR4/oilrrt.c|]



