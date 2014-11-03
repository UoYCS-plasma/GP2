module Cassava.Instructions where

type Prog = [Instr]

type Addr = Int
type Deg  = Int

data Instr = 
    ----------------------------------
    --  Node and edge finding prims
    ----------------------------------

    -- Find non-interface nodes
      NFI Deg
    | NFO Deg
    | NFL Deg

    -- Find interface nodes
    | INFI Deg
    | INFO Deg
    | INFL Deg

    -- Find root non-interface nodes
    | RNFI Deg
    | RNFO Deg
    | RNFL Deg

    -- Find root interface nodes
    | RINFI Deg
    | RINFO Deg
    | RINFL Deg

    | NCI Deg
    | NCO Deg
    | NCL Deg

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


