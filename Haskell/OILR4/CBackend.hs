module OILR4.CBackend where

import OILR4.IR
import OILR4.Config
import OILR4.Instructions
import OILR4.CRuntime
import OILR4.Spaces

import Mapping

import Data.List
import Data.Bits
import Debug.Trace


compileC :: OilrConfig -> Prog -> Maybe [Instr] -> String
compileC cf prog host = concat [preamble, cRuntime, spaces, chost, decls, defs]
    where defs   = concatMap compileDefn prog
          decls  = concatMap compileDecl prog
          spaces = if UseCompactIndex `elem` compilerFlags cf
                        then ( concatMap compileSS $ packedSpaces cf) ++ compileIndexMap cf
                        else ( concatMap compileSS $ searchSpaces cf)
          preamble = makePreamble cf
          chost  = case host of
                      Just h  -> compileHost h
                      Nothing -> ""

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
          globalOpts EnableExecutionTrace    = "#define OILR_EXECUTION_TRACE\n"
          globalOpts UseCompactIndex         = concat [ "#define OILR_COMPACT_INDEX\n"
                                                      , "#define OILR_PHYS_INDEX_SIZE ", show $ physIndCount cf, "\n"]
          globalOpts _ = ""

-- Compile a host graph
compileHost :: [Instr] -> String
compileHost is = concat [ "\n\n", decl "_HOST", " {\n", build [ "addNodes", show nNodes], concat cs, compileIns RET, "\n\n" ]
    where (nNodes, cs) = foldr makeHostElem (0, []) is
          makeHostElem (ABN _)     (n, cs) = (n+1, cs)
          makeHostElem (ABE _ s t) (n, cs) = (n, build ["addEdgeById", show s, show t]:cs)
          makeHostElem (RBN id v)  (n, cs) = (n, build ["setRootById", show id]:cs)
          makeHostElem NOP         (n, cs) = (n, cs)
          makeHostElem i           (n, cs) = error $ "Don't know how to host-compile " ++ show i
          makeNodes    _           (n, cs) = (n, cs)

-- Pre-declarations required for C, as we may have mutually recursive procs
compileDecl :: Definition -> String
compileDecl (name, _) = decl name ++ ";\n"

compileDefn :: Definition -> String
compileDefn (name, (pre, RuleBody lhs rhs, post)) = concat $
    ('\n':'\n':(decl name ++ " {\n\toilrCurrentRule=" ++ show name ++ ";\n")):[ compileIns i
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

compileIns (RBN dst bool)    = build ["RBN", show dst, if bool then "1" else "0"]

compileIns (CBL reg c)       = build ["CBN", show reg, show c]
compileIns (LBL dst n)       = error "Compilation not implemented"

compileIns (BND dst ss)      = build ["BND", show dst, spcName ss]
compileIns (BOE dst src tgt) = build ("BOE":[show n|n<-[dst,src,tgt]])
compileIns (BED dst r0 r1)   = build ("BED":[show n|n<-[dst,r0,r1]])
compileIns (BON d0 d1 src)   = build ("BON":[show n|n<-[d0,d1,src]])
compileIns (BIN d0 d1 tgt)   = build ("BIN":[show n|n<-[d0,d1,tgt]])
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

compileIns (ASRT ss n) = build ["ASRT", spcName ss]

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
                              , "long oracle_", name, ";\n"
                              , "long ",   name, "_pos;\n"]
    where name = spcName id

compileIndexMap :: OilrConfig -> String
compileIndexMap cf = concat [ "\tDList *indexMap[] = {"
                            , intercalate ", " $ map showMapping [0..(indexCount cf)-1]
                            , "};\n"]
    where showMapping i = concat ["&g.idx[", show (definiteLookup i l2p), "]"]
          l2p = logicalToPhys cf


{- compileIndexMap cf = concat [ "\tDList *indexMap[] = {"
                           , intercalate ", " $ map showMapping [0..(indexCount cf)-1]
                           , "};\n"]
    where showMapping i = concat ["&g.idx[", m i, "]"]
          m x = if x `elem` unused
                    then "0"
                    else show x
          unused = unusedInds n $ searchSpaces cf
-}
build :: [String] -> String
build (ins:args) = '\t':concat [ins, "(", intercalate "," args, ");\n"]

indName :: Int -> String
indName n = concat [ "&g.idx[", show n , "]"]

spcName :: Int -> String
spcName n = concat [ "ss_", show n ]

getIndSize :: OilrIndexBits -> Int
getIndSize (OilrIndexBits b c o i l r) = (1 `shift` (b+c+o+i+l+r))


