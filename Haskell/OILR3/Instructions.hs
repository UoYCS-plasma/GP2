module OILR3.Instructions where

type Tid = Int -- Trav id

data Instr = 
    -- Trav stack management
      DROT                  -- Drop Trav
    | CLRT                  -- Clear Trav stack
    -- Graph manipulation
    | ADN                   -- Add Node without Trav
    | ADE Int Int           -- Add Edge between Nodes
    | RTN Int               -- Set root flag on node

    | ANT                   -- Add Node and push Trav
    | AET                   -- Add Edge between top two Travs
    | DNT                   -- Delete Node in Trav
    | DET                   -- Delete Edge in Trav
    | DNE                   -- Delete Node and Edge in Trav
    -- Stack machine prims
    | LIT Int               -- push literal on data stack
    | ADD                   -- add top two values on ds
    | SUB                   -- subtract top of stack from next on stack
    | SHL                   -- shift NoS left by ToS bits
    -- Definition
    | DEF String
    | END
    -- Graph search
    -- flow control
    | CAL String | ALP String -- call rule or proc once or as-long-as-possible
    | RET                   -- unconditinoal return from current rule or proc
    -- logical operators
    | TRU  | FLS            -- set status register to true or false respectively
    deriving (Show, Eq)
