module OILR3.Instructions where

import OILR3.IR

import GPSyntax   -- for colours
import Mapping    -- for mappings

type Tid = Int -- Trav id

data Flag = DisableOilr | DisableSearchPlan | OilrInstructions | RecursiveRules | EnableDebugging | EnableParanoidDebugging | EnableExecutionTrace | Compile32Bit | CompactLists deriving (Eq, Show)

data Dim = Equ Int | GtE Int deriving (Show, Eq)

colourIds :: Mapping Colour Int
colourIds = [ (Uncoloured, 0)
            , (Red       , 1)
            , (Blue      , 2)
            , (Green     , 3)
            , (Grey      , 4) ]

colourMapping :: Mapping Colour Dim
colourMapping = (Any, GtE 0) : [ (c, Equ n) | (c, n) <- colourIds ]

-- TODO: this may not be adequate due to sort order of n-tuples, but it will do
-- for a first cut
instance Ord Dim where
    compare (Equ _) (GtE _) = GT
    compare (GtE _) (Equ _) = LT
    compare (Equ x) (Equ y) = compare x y
    compare (GtE x) (GtE y) = compare x y

data Pred = Pred { cDim :: Dim
                 , oDim :: Dim
                 , iDim :: Dim
                 , lDim :: Dim
                 , rDim :: Dim } 
    deriving (Show, Eq)

data Dir = In | Out | Either deriving (Eq, Show)

-- Match registers...
type Dst = Int
type Src = Int
type Tgt = Int
type End = Int

-- Other instr params
type Col = Int
type Spc = Int

type Target = String

-- Machine structure
--
-- Registers:  bool-flag   b-frame-pointer   match-reg-file


data Instr =
      OILR Int          -- Number of OILR indices
    -- Graph modification
    | ABN Dst           -- Add and Bind Node to register Dst
    | ABE Dst Src Tgt   -- Add and Bind Edge to register Dst between nodes in Src & Tgt
    | DBN Dst           -- Delete Bound Node 
    | DBE Dst           -- Delete Bound Node
    
    | RBN Dst Bool      -- set Root on Bound Node to Bool
    
    | CBL Dst Col       -- Colour Bound eLement
    | LBL Dst Int       -- Label Bound eLement with Int

    -- Graph search
    | BND Dst Spc          -- Bind next unbound NoDe in Spc to Dst
    | BED Dst End End Dir  -- Bind EDge between Ends in Dir
    | BEN Dst Dst Src Dir  -- Bind Edge and Node by following an edge in Dir from Src
    | NEC Src Tgt          -- Negative Edge Condition from Src to Tgt

    -- Definitions & program structure
    | TAR Target           -- jump TARget
    | BRZ Target           -- BRanch if Zero
    | CAL Target           -- CALl Target, pushing current IP to call-stack
    | RET                  -- RETurn to IP on top of call-stack

    -- Backtracking
    | CCP                  -- Create CheckPoint for backtracking
    | BAK                  -- roll-BAcK to last checkpoint
    | COM                  -- COMmit changes and discard top b-frame

    -- Stack machine
    | BLL Dst              -- push Bound eLement Label to stack
    | BLC Dst              -- push Bound eLement Colour to stack
    | BLR Dst              -- push Bound eLement Rootedness to stack

    -- Misc
    | NOP                  -- No-op



compileProg :: [OilrIR] -> [[Instr]]
compileProg = map compileProc



compileProc :: OilrIR -> [Instr]
compileProc (IRProc name e) = TAR (mangle name) : compileExpr e

{- data OilrExpr = IRSeqn [OilrExpr]
              | IRSet [Id]
              | IRBran OilrExpr OilrExpr
              | IRTrns OilrExpr   -- transaction that rolls-back if OilrExpr fails
              | IRDscd OilrExpr -- "discard" -- transaction that always rolls-back
              | IRCall Id | IRLoop OilrExpr
              | IRTrue | IRFals
     deriving (Show, Eq) -}


compileExpr :: OilrExpr -> [Instr]
compileExpr (IRSet rs)     = compileSet rs
compileExpr (IRBran th el) = compileBranch th el
compileExpr (IRTrns e)     = compileTrns e
compileExpr (IRDscd e)     = compileDscd e
compileExpr (IRCall id)    = compileCall (mangle id)
compileExpr (IRLoop e)     = compileLoop e
compileExpr IRTrue         = [ TRU ]
compileExpr IRFals         = [ FAL ]


compileSet :: [] -> [Instr]
compileSet = error "dong"

compileBranch :: OilrExpr -> OilrExpr -> [Instr]
compileBranch th el = 



mangle :: String -> String
mangle "Main"  = "OILR_Main"
mangle s@(i:_) | i `elem` ['A'..'Z'] = "OILR_proc_" ++ s
               | otherwise           = "OILR_rule_" ++ s



