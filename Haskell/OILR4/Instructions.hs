module OILR4.Instructions where

import Data.List
import Data.Bits

import OILR4.IR
import OILR4.Spaces

import GPSyntax   -- for colours
import Mapping    -- for mappings

import Debug.Trace


colourIds :: Mapping Colour Int
colourIds = [ (Uncoloured, 0)
            , (Red       , 1)
            , (Blue      , 2)
            , (Green     , 3)
            , (Grey      , 4) ]
edgeColourIds :: Mapping Colour Int
edgeColourIds = [ (Uncoloured, 0) , (Dashed, 1) ]

-- data Dir = In | Out | Either deriving (Eq, Show)

-- Match registers...
type Dst = Int
type Src = Int
type Tgt = Int
type Reg = Int

-- Other instr params
type Col = Int

type Target = String


-- Machine structure
--
-- Registers:  bool-flag   b-frame-pointer   match-reg-file

data Instr =
      OILR Int          -- Number of OILR indices
    | REGS Int          -- Size of local register file
    | SUC               -- Match success. Clean up after matching, and possibly recurse
    | UBN Int           -- UnBiNd elements in n registers (possibly superfluous?)
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
    | BED Dst Reg Reg      -- Bind EDge from Reg1 to Reg2
    | BON Dst Dst Src      -- Bind Edge and Node by following an edge from Src
    | BIN Dst Dst Src      -- Bind Edge and Node by following an edge from Src
    | BLO Dst Reg          -- Bind a LOop on node in Reg
    | NEC Src Tgt          -- Negative Edge Condition from Src to Tgt

    -- Definitions & program structure
    | DEF Id               -- DEFine function Idopen source dev site
    | CAL Id               -- CALl Id, pushing current IP to call-stack
    | TAR Target           -- jump TARget
    | BRZ Target           -- BRanch if Zero (i.e. if bool flag is false)
    | BNZ Target           -- Branch if Non-Zero
    | BRA Target           -- Branch RAndomly. Take the branch 50% of the time.
    | BRN Target           -- unconditional BRaNch to Target
    | RET                  -- RETurn to IP on top of call-stack
    | RTZ                  -- ReTurn if Zero
    | RNZ                  -- Return if Non-Zero

    -- Backtracking
    | BBT                  -- Begin BackTracking section
    | BAK                  -- unconditionally roll-BAcK backtracking section changes
    | EBT                  -- End BackTracking secion: commit if flag is true, rollback otherwise
    -- There is no rollback command. This needs to be done manually with reverse rules.

    -- Stack machine
    -- | BLO Dst              -- push Bound eLement Out-degree to stack
    | BLI Dst              -- push Bound eLement In-degree to stack
    | BLL Dst              -- push Bound eLement looP-degree to stack
    | BLR Dst              -- push Bound eLement Rootedness to stack
    | BLN Dst              -- push Bound eLement's Numeric label to stack
    | BLC Dst              -- push Bound eLement Colour to stack

    | SHL Int              -- SHift top-of-stack Left by Int bits
    | OR                   -- bitwise OR top 2 values on the stack
    | AND                  -- bitwise AND top 2 value on the stack

    -- Misc
    | NOP                  -- No-OP
    | TRU                  -- set the boolean register to TRUe
    | FLS                  -- set the boolean register to FaLSe
    deriving (Show, Eq)

prettyPrint :: [[Instr]] -> String
prettyPrint iss = concat $ concat [[ prettyPrintIns i ++ "\n" | i <- is ] | is <- iss ]

prettyPrintIns (DEF n) = '\n' : n ++ ":"
prettyPrintIns (TAR n) = n ++ ":"
prettyPrintIns i       = '\t' : show i


compileProg :: [OilrIR] -> [[Instr]]
compileProg = map compile

compile :: OilrIR -> [Instr]
compile (IRProc name e)  = DEF (mangle name) : compileExpr 0 e  ++  [RET]
compile (IRRule name es) = DEF (mangle name) : compileRule es

{- data OilrExpr = IRSeqn [OilrExpr]
              | IRRuleSet [Id]
              | IRBran OilrExpr OilrExpr
              | IRTrns OilrExpr   -- transaction that rolls-back if OilrExpr fails
              | IRDscd OilrExpr -- "discard" -- transaction that always rolls-back
              | IRCall Id | IRLoop OilrExpr
              | IRTrue | IRFals
     deriving (Show, Eq) -}

{- data OilrElem = IRNode  Id  Colour  IRLabel  Bool
              | IREdge  Id  Colour  IRLabel  Bool  Id Id
              | IREql   IRLabel IRLabel
              | IRNot   OilrElem
              | IRNothing -}

-- Compile a rule definition
compileRule :: OilrRule -> [Instr]
compileRule ms = REGS (length regs) : reverse (SUC:lhs) ++ reverse (RET:UBN (length regs):rhs)
    where (regs, lhs, rhs) = foldr compileMod ([], [], []) $ reverse ms


sortRule :: OilrRule -> OilrRule
sortRule = sortBy mostConstrained

mostConstrained :: OilrMod OilrElem -> OilrMod OilrElem -> Ordering
-- created nodes are least constrained
mostConstrained (Create IRNothing) _                   = LT
mostConstrained _                  (Create IRNothing)  = GT
-- deleted nodes are most constrained
mostConstrained (Delete a) (Delete b) = mostConstrainedElem a b
mostConstrained (Delete _) _          = GT
mostConstrained _          (Delete _) = LT
-- from here it gets woollier...
mostConstrained (Change a _) (Change b _) = mostConstrainedElem a b
mostConstrained (Same a)     (Same b)     = mostConstrainedElem a b
mostConstrained (Change a _) (Same b)     = mostConstrainedElem a b
mostConstrained (Same a)     (Change b _) = mostConstrainedElem a b


mostConstrainedElem :: OilrElem -> OilrElem -> Ordering
mostConstrainedElem (IRNode _ _ _ lOilr) (IRNode _ _ _ rOilr) = compare lOilr rOilr
mostConstrainedElem a b = error $ "Don't know how to compare " ++ show a ++ " with " ++ show b


compileMod :: (OilrMod OilrElem) -> (Mapping Id Int, [Instr], [Instr]) -> (Mapping Id Int, [Instr], [Instr])
compileMod (Create x) (regs, lhs, rhs) = case x of
    (IRNode id _ _ _     ) -> ( (id,r):regs, lhs                  , ABN r:rhs )
    (IREdge id _ _ _ s t ) -> ( (id,r):regs, lhs                  , abe regs r s t:rhs )
    where r = length regs
compileMod (Delete x) (regs, lhs, rhs) = case x of
    (IRNode id _ _ _)      -> ( (id,r):regs, BND r spc:lhs          , DBN r:rhs )
    (IREdge id _ _ bi s t) -> ( (id,r):regs, bed regs r s t bi:lhs, DBE r:rhs )
    where r = length regs
          spc = makeSpc (Delete x)
compileMod (Same x)   (regs, lhs, rhs) = case x of
    IRNode id _ _ _      -> ((id, r):regs, BND r spc:lhs            , rhs)
    IREdge id _ _ bi s t -> ((id, r):regs, bed regs r s t bi:lhs  , rhs)
    where r = length regs
          spc = makeSpc (Same x)
compileMod (Change left right) (regs, lhs, rhs) = case (left, right) of
    (IRNode id _ _ _     , IRNode _ _ _ _)
                         -> ( (id,r):regs, BND r (makeSpc $ Change left right):lhs            , diffs regs r left right ++ rhs )
    (IREdge id _ _ bi s t, IREdge _ _ _ _ _ _)
                         -> ( (id,r):regs, bed regs r s t bi:lhs  , diffs regs r left right ++ rhs )
    where r = length regs
compileMod (Check (IRNot (IREdge id _ _ _ s t))) (regs, lhs, rhs) = (regs, nec regs s t:lhs, rhs)
-- compileMod x _ = error $ show x

diffs :: Mapping Id Int -> Reg -> OilrElem -> OilrElem -> [Instr]
diffs regs r (IRNode ib cb lb (Sig _ _ _ rb)) (IRNode ia ca la (Sig _ _ _ ra)) =
    concat [ if cb /= ca then [CBL r $ definiteLookup ca colourIds] else []
           , if lb /= la then [LBL r 0] else []     -- TODO: label support
           , if rb /= ra then [RBN r ra] else [] ]
diffs regs r (IREdge ib cb lb bb sb tb) (IREdge ia ca la ba sa ta)
    -- i = id, c = colour, l = label, b = bidi, s = source node, t = target node
    | sb == sa && tb == ta =
        case bb == ba || ba of 
        True -> concat [ if cb /= ca then [CBL r $ definiteLookup ca edgeColourIds] else []
                       , if lb /= la then [LBL r 0] else [] ]  -- TODO: label support
        False -> [ DBE r, abe regs r sa ta, CBL r $ definiteLookup ca edgeColourIds, LBL r 0] -- TODO: label support
    | otherwise            = error "Edge source and target should not change"

bed :: Mapping Id Reg -> Reg -> Id -> Id -> Bool -> Instr
bed regs r s t bidi = BED r (definiteLookup s regs) (definiteLookup t regs)

abe :: Mapping Id Reg -> Reg -> Id -> Id -> Instr
abe regs r s t = ABE r (definiteLookup s regs) (definiteLookup t regs)

nec :: Mapping Id Reg -> Id -> Id -> Instr
nec regs s t = NEC (definiteLookup s regs) (definiteLookup t regs)

compileExpr :: Int -> OilrExpr -> [Instr]
compileExpr i (IRRuleSet rs)       = compileSet (i+1) rs
compileExpr i (IRIf  cn th el) = concat [ compileExpr (i+1) cn
                                        , [ brz i "elseI", BAK ]
                                        , compileExpr (i+1) th
                                        , [ brn i "endI" , tar i "elseI" ]
                                        , compileExpr (i+1) el
                                        , [ tar i "endI" ] ]
compileExpr i (IRTry cn th el) = concat [ compileExpr (i+1) cn 
                                        , [ brz i "elseT" ]
                                        , compileExpr (i+1) th
                                        , [ brn i "endT" , tar i "elseT" ]
                                        , compileExpr (i+1) el
                                        , [ tar i "endT" ] ]
compileExpr i (IRTrns e)     = concat [ [BBT] , compileExpr (i+1) e , [EBT] ]
compileExpr i (IRSeqn es)    = compileSequence (i+1) es
compileExpr i (IRLoop e)     = concat [ [ tar i "bgn"],
                                        compileExpr (i+1) e,
                                        [bnz i "bgn", TRU] ]
compileExpr i (IRCall id)    = [ CAL (mangle id) ]
compileExpr i IRTrue         = [ TRU ]
compileExpr i IRFals         = [ FLS ]


compileSequence :: Int -> [OilrExpr] -> [Instr]
compileSequence i es = concat [ compileExpr (i+1) e | e <- es ]

compileSet :: Int -> [Id] -> [Instr]
compileSet i [r] = [ CAL (mangle r) ]
compileSet i rs = intercalate [(bnz i "end")] [ [CAL (mangle r)] | r <- rs ] ++ [tar i "end"]


mangle :: String -> String
mangle "Main"  = "OILR_Main"
mangle s@(i:_) | i `elem` ['A'..'Z'] = "OILR_proc_" ++ s
               | otherwise           = "OILR_rule_" ++ s

bnz = labelledInstr BNZ
bra = labelledInstr BRA
brn = labelledInstr BRN
brz = labelledInstr BRZ
tar = labelledInstr TAR

labelledInstr :: (String -> Instr) -> Int -> String -> Instr
labelledInstr ins i s = ins ( s ++ ('_':show i) )

