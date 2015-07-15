module OilrMachine.CBackend where

import OilrMachine.Instructions
import Data.List

type CSrc = String
type CHdr = String

stringifyPredicate :: String -> String
stringifyPredicate p = p

stringifyArgs :: (Show a) => [a] -> String
stringifyArgs vs = "(" ++ (concat . intersperse "," . map show) vs ++ ")"

hCompileI :: Instr -> CHdr
hCompileI (OILR o i l r) = 
        concat $ intersperse "\n" ["#define NDEBUG" , "",
                                   "#define O_BITS " ++ show o,
                                   "#define I_BITS " ++ show i,
                                   "#define L_BITS " ++ show l,
                                   "#define R_BITS " ++ show r, ""]
hCompileI _ = ""

cCompileI :: Instr -> CSrc
cCompileI (OILR o i l r ) = "    OILR" ++ stringifyArgs [o, i, l, r]

cCompileI (SPC n)         = "    SPC"    ++ stringifyArgs [n]
cCompileI (TRAV a b pred) = "    TRAV"   ++ stringifyArgs [a, b, pred]
cCompileI (ORFAIL n)      = "    ORFAIL" ++ stringifyArgs [n]
cCompileI (ORBACK n)      = "    ORBACK" ++ stringifyArgs [n]

cCompileI (NODE t)       = "    NODE" ++ stringifyArgs [t]
cCompileI (EDTO src t)   = "    EDTO" ++ stringifyArgs [src, t]
cCompileI (EDFR tgt t)   = "    EDFR" ++ stringifyArgs [tgt, t]
cCompileI (EDNO src tgt) = "    EDNO" ++ stringifyArgs [src, tgt]

cCompileI (RESET t)   = "    RESET" ++ stringifyArgs [t]

cCompileI (PROC name) = "    PROC(" ++ name ++ ")"
cCompileI (ALAP proc) = "    ALAP(" ++ proc ++ ")"

cCompileI i | i `elem` [ MTCH, ENDM ] = "    " ++ show i
            | otherwise = show i

{-
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
-}

cCompile :: Prog -> (CSrc, CHdr)
cCompile prog = ( "#include \"oilrinst.h\"\n#include \"oilrrt.h\"\n\n" ++ (concat . intersperse "\n" . map cCompileI) prog , concatMap hCompileI prog )

