module OILR4.CBackend where

import OILR4.IR
import OILR4.Config
import OILR4.Instructions
import OILR4.CRuntime

import Mapping

import Data.List
import Data.Bits
import Debug.Trace


compileC :: OilrConfig -> Prog -> String
compileC cf prog = concat [preamble, cRuntime, spaces, decls, defs]
    where defs   = concatMap compileDefn prog
          decls  = concatMap compileDecl prog
          spaces = concatMap compileSS $ searchSpaces cf
          preamble = makePreamble cf

makePreamble :: OilrConfig -> String
makePreamble cf = concat [ trace (show flags) $ concatMap globalOpts flags, "\n",
                           "#define OILR_B_BITS ", show $ bBits inds , "\n" ,
                           "#define OILR_C_BITS ", show $ cBits inds , "\n" ,
                           "#define OILR_O_BITS ", show $ oBits inds , "\n" ,
                           "#define OILR_I_BITS ", show $ iBits inds , "\n" ,
                           "#define OILR_L_BITS ", show $ lBits inds , "\n" ,
                           "#define OILR_R_BITS ", show $ rBits inds , "\n" ]
    where inds = indBits
          flags = compilerFlags cf
          globalOpts EnableDebugging         = "#define OILR_DEBUGGING\n"
          globalOpts EnableParanoidDebugging = "#define OILR_PARANOID_CHECKS\n"
          globalOpts NoRecursion             = "#define MAX_RECURSE 0\n"
          globalOpts _ = ""

-- Pre-declarations required for C, as we may have mutually recursive procs
compileDecl :: Definition -> String
compileDecl (name, _) = decl name ++ ";\n"

compileDefn :: Definition -> String
compileDefn (name, (pre, RuleBody lhs rhs, post)) = concat $
    ('\n':'\n':(decl name ++ " {\n")):[ compileIns i
                        | i <- concat [pre, lhs, rhs, post] ]
compileDefn (name, (pre, ProcBody is, post)) = concat $
    ('\n':'\n':(decl name ++ " {\n")):[ compileIns i
                        | i <- concat [pre, is, post] ]


decl :: String -> String
decl name = concat [ "void ", name, "()" ]

-- compileIns (OILR n) = error "Compilation not implemented"
-- compileIns (DEF name) = error "Compilation not implemented"
compileIns (ONCE name)       = build ["ONCE", name]
compileIns (ALAP name)       = build ["ALAP", name]

compileIns (REGS n)          = build ["REGS", show n]
-- compileIns (SUC) = error "Compilation not implemented"
compileIns (UBN n)           = build ["UBN", show n]
compileIns (RST ss)          = build ["RST", spcName ss]

compileIns (ABN dst)         = build ["ABN", show dst]
compileIns (ABE dst src tgt) = build ("ABE":[show n|n<-[dst,src,tgt]])
compileIns (ABL dst src)     = build ["ABL", show dst, show src]
compileIns (DBN reg)         = build ["DBN", show reg]
compileIns (DBE reg)         = build ["DBE", show reg]
compileIns (DBL reg)         = build ["DBL", show reg]

compileIns (RBN dst bool)    = error "Compilation not implemented"

compileIns (CBL reg c)       = build ["CBN", show reg, show c]
compileIns (LBL dst n)       = error "Compilation not implemented"

compileIns (BND dst ss)      = build ["BND", show dst, spcName ss]
compileIns (BOE dst src tgt) = build ("BOE":[show n|n<-[dst,src,tgt]])
compileIns (BED dst r0 r1)   = build ("BED":[show n|n<-[dst,r0,r1]])
compileIns (BON d0 d1 src)   = build ("BON":[show n|n<-[d0,d1,src]])
compileIns (BIN d0 d1 tgt)   = build ("BON":[show n|n<-[d0,d1,tgt]])
compileIns (BEN d0 d1 r0)    = error "Compilation not implemented"
compileIns (BLO dst r0)      = build ["BLO", show dst, show r0]
compileIns (NEC src tgt)     = build ["NEC", show src, show tgt]

compileIns (TAR t)           = t ++ ":\n"
compileIns (BRZ t)           = build ["BRZ", t]
compileIns (BNZ t)           = build ["BNZ", t]
compileIns (BRA t)           = build ["BRA", t]
compileIns (BRN t)           = build ["BRN", t]

compileIns (RET)             = "l_exit:\n\treturn;\n}"
-- compileIns (RTZ) = error "Compilation not implemented"
-- compileIns (RNZ) = error "Compilation not implemented"

-- compileIns (BBT) = error "Compilation not implemented"
-- compileIns (BAK) = error "Compilation not implemented"
-- compileIns (EBT) = error "Compilation not implemented"

compileIns (BLI dst) = error "Compilation not implemented"
compileIns (BLL dst) = error "Compilation not implemented"
compileIns (BLR dst) = error "Compilation not implemented"
compileIns (BLN dst) = error "Compilation not implemented"
compileIns (BLC dst) = error "Compilation not implemented"

compileIns (SHL n) = error "Compilation not implemented"
-- compileIns (OR) = error "Compilation not implemented"
-- compileIns (AND) = error "Compilation not implemented"

-- compileIns (NOP) = error "Compilation not implemented"
-- compileIns (TRU) = error "Compilation not implemented"
-- compileIns (FLS) = error "Compilation not implemented"

compileIns i     = build [show i]

compileSS (id, inds) = concat [ "\nDList *", name, "[] = { "
                              , intercalate ", " (map indName inds), ", NULL };\n"
                              , "DList *", name, "_dl;\n"
                              , "long ",   name, "_pos;\n"]
    where name = spcName id

build :: [String] -> String
build (ins:args) = '\t':concat [ins, "(", intercalate "," args, ");\n"]

indName :: Int -> String
indName n = concat [ "&g.idx[", show n , "]"]

spcName :: Int -> String
spcName n = concat [ "ss_", show n ]

getIndSize :: OilrIndexBits -> Int
getIndSize (OilrIndexBits b c o i l r) = (1 `shift` (b+c+o+i+l+r))


