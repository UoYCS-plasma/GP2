module Cassava.Instructions where

type Prog = [Instr]

type Addr = Int
type Deg  = Int

data Instr = 
    ----------------------------------
    --  Node and edge finding prims
    ----------------------------------

    -- Find non-interface nodes
      FN  Deg Deg Deg
    | FRN Deg Deg Deg

    -- Find interface nodes
    | FIN Deg Deg Deg
    | FRIN Deg Deg Deg

    -- Constrain in- and out-degrees to 
    -- exact number, even for interface
    -- nodes
    | NCI | NCO | NCL

    -- Get the next node
    | NEXT

    -- Find an edge from 2 nodes
    | EF

    -- Find two nodes from an edge
    | NFE


    ----------------------------------
    -- Program control prims
    ----------------------------------

    | PROC String

    | CALL String
    | RET

    | JUMP Addr
    | JS Addr
    | JF Addr

    | COMMIT

    ----------------------------------
    -- Stack management prims
    ----------------------------------

    | NDUP
    | NSWAP
    | NNIP
    | NDROP

    | EDUP
    | ESWAP
    | ENIP
    | EDROP

    | LDUP
    | LSWAP
    | LNIP
    | LDROP

    | TDUP
    | TSWAP
    | TNIP
    | TDROP

    | GDUP
    | GSWAP
    | GNIP
    | GDROP

    deriving (Show, Eq)


