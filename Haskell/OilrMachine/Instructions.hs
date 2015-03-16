module OilrMachine.Instructions where

type Prog = [Instr]

type Addr = Int
type Deg  = Int

data Instr = 
    ----------------------------------
    --  Node and edge finding prims
    ----------------------------------

    -- Settings for OILR index creation.
      OILR Deg Deg Deg Deg

    -- Add an element to a search plan
    | SPC Deg Deg Deg Deg

    -- Add a traverser
    | TRAV Deg Deg Deg Deg

    ----------------------------------
    -- Graph manipulation
    ----------------------------------

    -- delete a node in a TRAV register
    | DELN Int

    -- delete an edge in a TRAV register
    | DELE Int
    
    -- introduce a new edge between src and tgt
    | NEWE Int Int

    -- introduce a new node
    | NEWN

    -- Set and unset root flag on a node
    | ROOT Int | TOOR Int

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
    | ORFAIL
    -- if success flag is unset go back to an 
    -- earlier trav.
    | ORBACK

    ----------------------------------
    -- Miscellaneous prims
    ----------------------------------

    -- Unset success flag and return
    | FAIL

    -- Do nothing (i.e. "skip" in GP2 terms)
    | NOP

    deriving (Show, Eq)


