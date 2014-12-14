module Cassava.NullBackend where

import Cassava.Instructions
import Data.List

codeGen :: Prog -> String
codeGen = concat . intersperse "\n" . map codeGenForInstr

codeGenForInstr :: Instr -> String
codeGenForInstr = show




