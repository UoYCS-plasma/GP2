module OILR3.Instructions where

type Tid = Int -- Trav id

data Flag = DisableOilr | DisableSearchPlan | OilrInstructions | EnableDebugging | EnableParanoidDebugging | EnableExecutionTrace deriving (Eq, Show)

data Dim = Equ Int | GtE Int deriving (Show, Eq)

-- TODO: this may not be adequate due to sort order of n-tuples, but it will do
-- for a first cut
instance Ord Dim where
    compare (Equ _) (GtE _) = GT
    compare (GtE _) (Equ _) = LT
    compare (Equ x) (Equ y) = compare x y
    compare (GtE x) (GtE y) = compare x y

data Pred = Pred { oDim :: Dim
                 , iDim :: Dim
                 , lDim :: Dim
                 , rDim :: Dim } 
    deriving (Show, Eq)


data Instr a b = 
      OILR Int
    -- Graph manipulation
    | ADN a                 -- Add Node without Trav
    | ADE b a a             -- Add Edge between Nodes
    | DEN a                 -- Delete Node with id
    | DEE b                 -- Delete Edge with id
    | RTN a                 -- Set root flag on node
    | URN a                 -- unset root flag on node

    -- Stack machine prims
    | LIT Int               -- push literal on data stack
    | ADD                   -- add top two values on ds
    | SUB                   -- subtract top of stack from next on stack
    | SHL                   -- shift NoS left by ToS bits
    -- Definition
    | RUL String
    | PRO String
    | END
    -- Graph search
    -- | CRS a Pred            -- conditional reset of trav
    | LUN a Pred
    | LUE b a a
    | XIE a b a             -- extend match back along an incoming edge
    | XOE a b a             -- extend match along an outgoing edge 
    | NEC a a               -- no-edge condition
    | UBA                   -- unbind all
    -- flow control
    | CAL String | ALP String -- call rule or proc once or as-long-as-possible
    | RET                   -- unconditinoal return from current rule or proc
    | ORB a                 -- back to a if success flag is unset
    | ORF                   -- exit procedure if success flag is unset
    -- logical operators
    | TRU  | FLS            -- set status register to true or false respectively
    deriving (Show, Eq)
