module OILR4.X86Backend (compileX86) where

import OILR4.Instructions
import OILR4.Config

import Data.List


compileX86 :: OilrConfig -> Prog -> String
compileX86 cf prog = concat [spaces, defs]
    where defs   = concatMap compileDefn prog
          spaces = concatMap compileSS $ searchSpaces cf

compileDefn :: Definition -> String
compileDefn (name, (pre, RuleBody lhs rhs, post)) = intercalate "\n" $
    ("\n\nrule " ++ name):[ compileIns i
                        | i <- concat [pre, lhs, rhs, post] ]
compileDefn (name, (pre, ProcBody is, post)) = intercalate "\n" $
    ("\n\nproc " ++ name):[ compileIns i
                        | i <- concat [pre, is, post] ]



-- compileIns (OILR n) = error "Compilation not implemented"
-- compileIns (DEF name) = error "Compilation not implemented"
compileIns (CAL name)        = build [name]

compileIns (REGS n)          = build ["REGS", show n]
-- compileIns (SUC) = error "Compilation not implemented"
compileIns (UBN n)           = build ["UBN", show n]

compileIns (ABN dst)         = build ["ABN", show dst]
compileIns (ABE dst src tgt) = build ("ABE":[show n|n<-[dst,src,tgt]])
compileIns (DBN reg)         = build ["DBN", show reg]
compileIns (DBE reg)         = build ["DBE", show reg]

compileIns (RBN dst bool)    = error "Compilation not implemented"

compileIns (CBL reg c)       = build ["CBN", show reg, show c]
compileIns (LBL dst n)       = error "Compilation not implemented"


compileIns (BND dst ss)      = build ["BND", show dst, spc_name ss]
compileIns (BOE dst src tgt) = build ("BOE":[show n|n<-[dst,src,tgt]])
compileIns (BED dst r0 r1)   = build ("BED":[show n|n<-[dst,r0,r1]])
compileIns (BON d0 d1 src)   = error "Compilation not implemented"
compileIns (BIN d0 d1 tgt)   = error "Compilation not implemented"
compileIns (BEN d0 d1 r0)    = error "Compilation not implemented"
compileIns (BLO dst r0)      = error "Compilation not implemented"
compileIns (NEC src tgt)     = build ["NEC", show src, show tgt]

compileIns (TAR t)           = t ++ ":"
compileIns (BRZ t)           = build ["BRZ", branch_offs t]
compileIns (BNZ t)           = build ["BNZ", branch_offs t]
compileIns (BRA t)           = build ["BRA", branch_offs t]
compileIns (BRN t)           = build ["BRN", branch_offs t]

-- compileIns (RET) = error "Compilation not implemented"
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


compileSS (id, inds) = concat [ "\n", spc_name id, ":\n\t.long 0,0\n\t.long "
                              , intercalate "," (map show inds), "\n"]

build :: [String] -> String
build ss = concat ("\t.long ":intersperse ", " ss)

spc_name :: Int -> String
spc_name n = concat [ "ss_", show n ]

branch_offs :: String -> String
branch_offs t = concat ["JUMP(", t, ")"]

