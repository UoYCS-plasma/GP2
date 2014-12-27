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

    -- Create a non-interface node traverser
    | TN  Deg Deg Deg
    | TRN Deg Deg Deg

    -- Create an interface node traverser
    | TIN Deg Deg Deg
    | TRIN Deg Deg Deg

    -- Constrain out-degree, in-degree or 
    -- loopiness to an exact number (for
    -- interface node traverser). Int is 
    -- TRAV register number containing the
    -- traverser.
    | FIXO Int | FIXI Int | FIXL Int

   -- Create an edge traverser from
    -- node traversers in first reg to
    -- node traverser in second reg.
    | TE Int Int

    -- Create an edge traverser for 
    -- any edge between traversers in
    -- TRAV registers ("Traverse bi-di 
    -- edges")
    | TBE Int Int

    -- Create a traverser-like object
    -- that succeeds if an edge exists
    | CE Int Int
    | CBE Int Int

    -- Create a traverser-like object
    -- that fails if its edge exists!
    | XE Int Int

    -- Run the search described by the 
    -- TRAV stack.
    | GO

    ----------------------------------
    -- Graph manipulation
    ----------------------------------

    -- delete all non-interface nodes in 
    -- TRAV registers
    | DELN

    -- delete all edges on TRAV
    | DELE
    
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
    | LOOP String
    -- return from a procedure
    | RET

    -- Clear TRAV and return on fail
    | ZTRF

{-    ----------------------------------
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
-}
    deriving (Show, Eq)


