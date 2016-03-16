module OILR4.X86Backend



compileIns (OILR n) = 
compileIns (REGS n) = 
compileIns (SUC) = 
compileIns (UBN n) = 

compileIns (ABN dst) = 
compileIns (ABE dst src tgt) = 
compileIns (DBN dst) = 
compileIns (DBE dst) = 

compileIns (RBN dst bool) = 

compileIns (CBL dst Col) = 
compileIns (LBL dst n) = 


compileIns (BND dst ss) = 
compileIns (BOE dst src tgt) = 
compileIns (BED dst r0 r1) = 
compileIns (BON d0 d1 src) = 
compileIns (BIN d0 d1 tgt) = 
compileIns (BEN d0 d1 r0) = 
compileIns (BLO dst r0) = 
compileIns (NEC src tgt) = 


compileIns (DEF name) = 
compileIns (CAL name) = 
compileIns (TAR t) = 
compileIns (BRZ t) = 
compileIns (BNZ t) = 
compileIns (BRA t) = 
compileIns (BRN t) = 
compileIns (RET) = 
compileIns (RTZ) = 
compileIns (RNZ) = 


compileIns (BBT) = 
compileIns (BAK) = 
compileIns (EBT) = 




compileIns (BLI dst) = 
compileIns (BLL dst) = 
compileIns (BLR dst) = 
compileIns (BLN dst) = 
compileIns (BLC dst) = 

compileIns (SHL n) = 
compileIns (OR) = 
compileIns (AND) = 


compileIns (NOP) = 
compileIns (TRU) = 
compileIns (FLS) = 
