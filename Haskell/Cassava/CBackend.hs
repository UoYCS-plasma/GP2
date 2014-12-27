module Cassava.CBackend where

import Cassava.Instructions
import Data.List


stringifyArgs :: (Show a) => [a] -> String
stringifyArgs vs = "(" ++ (concat . intersperse "," . map show) vs ++ ")"

cCompileI :: Instr -> String
cCompileI (OILR o i l r ) = "    OILR" ++ stringifyArgs [o, i, l, r]

cCompileI (TN o i l)  = "    TN" ++ stringifyArgs [o, i, l]
cCompileI (TRN o i l) = "    TRN" ++ stringifyArgs [o, i, l]
cCompileI (TIN o i l) = "    TIN" ++ stringifyArgs [o, i, l]
cCompileI (TRIN o i l) = "    TRIN" ++ stringifyArgs [o, i, l]

cCompileI (TE r s)  = "    TE"  ++ stringifyArgs [r, s]
cCompileI (TBE r s) = "    TBE" ++ stringifyArgs [r, s]
cCompileI (CE r s)  = "    CE"  ++ stringifyArgs [r, s]
cCompileI (CBE r s) = "    CBE" ++ stringifyArgs [r, s]
cCompileI (XE r s)  = "    XE"  ++ stringifyArgs [r, s]
cCompileI (NEWE a b) = "    NEWE" ++ stringifyArgs [a, b]

cCompileI (FIXO r) = "    FIXO" ++ stringifyArgs [r]
cCompileI (FIXI r) = "    FIXI" ++ stringifyArgs [r]
cCompileI (FIXL r) = "    FIXL" ++ stringifyArgs [r]
cCompileI (ROOT r) = "    ROOT" ++ stringifyArgs [r]
cCompileI (TOOR r) = "    TOOR" ++ stringifyArgs [r]

cCompileI (PROC s) = "PROC(" ++ s ++ ")"
cCompileI (CALL s) = "    CALL(" ++ s ++ ")"
cCompileI (LOOP s) = "    LOOP(" ++ s ++ ")"


cCompileI i | i `elem` [GO, DELE, DELN, ZTRF, NEWN] = "    " ++ show i
            | otherwise = show i



cCompile :: Prog -> String
cCompile prog = "#include \"oilrinst.h\"\n#include \"oilrrt.h\"\n\n" ++ (concat . intersperse "\n" . map cCompileI) prog

