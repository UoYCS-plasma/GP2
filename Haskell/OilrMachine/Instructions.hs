module OilrMachine.Instructions where

type Prog = [Instr]

type Addr = Int
type Deg  = Int
type Sig  = Int
type Bits = Int
type SpId = Int
type TrId = Int
type PrId = Int

data Instr = 
      OILR Bits Bits Bits Bits
    
    ----------------------------------
    --  Node and edge finding prims
    ----------------------------------

    -- Add an element to a search plan
    | PLANS | ENDP
    | SPC Sig

    -- Add a traverser
    | TRAVS | ENDT
    | TRAV SpId SpId PrId

    -- Start and end a match
    | MTCH | ENDM

    | NODE TrId | EDTO TrId TrId | EDFR TrId TrId | EDNO TrId TrId

    | RESET TrId

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

    | PROC String

    -- Call a rule or procedure
    | CALL String

    -- Looped-call a rule or procedure
    | ALAP String

    -- return from a procedure
    | RET

    -- if success flag is unset exit current proc
    | ORFAIL TrId
    -- if success flag is unset reset specified Trav 
    -- and go back to the previous trav
    | ORBACK TrId

    ----------------------------------
    -- Miscellaneous prims
    ----------------------------------

    -- Unset success flag and return
    | FAIL

    -- Do nothing (i.e. "skip" in GP2 terms)
    | NOP

    deriving (Show, Eq)


