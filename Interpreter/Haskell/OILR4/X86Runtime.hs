{-# LANGUAGE QuasiQuotes #-}
module OILR4.X86Runtime where

import OILR4.EmbedFileContents

x86Runtime :: String
x86Runtime = [litFile|OILR4/oilrrt.S|]



