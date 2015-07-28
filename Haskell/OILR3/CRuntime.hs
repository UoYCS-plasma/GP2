{-# LANGUAGE QuasiQuotes #-}
module OILR3.CRuntime where

import OILR3.EmbedFileContents

cRuntime :: String
cRuntime = [litFile|OILR3/oilrrt.c|]



