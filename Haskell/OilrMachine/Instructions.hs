module OilrMachine.Instructions where

type Prog = [Instr]

type Spc = Int
type Sig = Int
type Tid = Int
type Off = Int

type Rid = String

data Instr = 
    
    ----------------------------------
    --  Node and edge finding prims
    ----------------------------------

    | LKU Spc      -- Lookup a node in the indices in Spc
    | XTO Tid Sig  -- Extend match along out-edge from node in Tid to node with Sig
    | XFR Tid Sig  -- Extend match along in-edge from node with Sig to node in Tid
    | NEC Tid Tid  -- Negative Edge Condition between nodes in Tids
    | NNC Spc      -- Negative node condition: Spc must be empty
    | NOX Tid Sig  -- Negative extension cond: no outgoing edge to node with Sig
    | NIX Tid Sig  -- Negative extension cond: no incoming edge from node with Sig

    | RST Tid      -- Reset Tid

    ----------------------------------
    -- Graph manipulation
    ----------------------------------

    -- delete a node in a TRAV register
    | DELN TrId

    -- delete an edge in a TRAV register
    | DELE TrId
    
    -- introduce a new edge between src and tgt TRAVS
    | NEWE TrId TrId

    -- introduce a new node, storing it in a dummy trav
    | NEWN TrId

    -- Set and unset root flag on a node held in a Trav
    | ROOT TrId | TOOR TrId

    ----------------------------------
    -- Program control prims
    ----------------------------------

    | ALP Rid   -- Repeat rule or proc As Long as Possible
    | CAL Rid   -- Call rule or proc Rid
    | RET       -- Unconditional return from rule or proc
    | ZRT       -- Return on failure
    | ZBR Off   -- Branch relative on failure


    ----------------------------------
    -- Miscellaneous prims
    ----------------------------------

    -- Unset success flag and return
    | FAIL

    -- Do nothing (i.e. "skip" in GP2 terms)
    | NOP

    deriving (Show, Eq)


