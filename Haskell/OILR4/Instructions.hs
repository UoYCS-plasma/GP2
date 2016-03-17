module OILR4.Instructions where

import Data.List
import Data.Bits

import OILR4.Config
import OILR4.IR
import OILR4.Spaces

import Mapping    -- for mappings

import Debug.Trace


-- Match registers, for readability
type Dst = Int
type Src = Int
type Tgt = Int
type Reg = Int
type Sid = Int   -- Search Space Id

-- Other instr params
type Col = Int

type Target = String


-- Machine structure
--
-- Registers:  bool-flag   b-frame-pointer   match-reg-file

type Prog = [Definition]
type Definition = ( String,  ([Instr], DefBody, [Instr]) )
data DefBody = ProcBody [Instr]
             --          lhs     rhs
             | RuleBody [Instr] [Instr]
    deriving (Show, Eq)

data Instr =
      OILR Int          -- Number of OILR indices
    | REGS Int          -- Size of local register file
    | SUC               -- Match success. Clean up after matchingthey, and possibly recurse
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
    | BND Dst Sid          -- Bind next unbound NoDe in Spc to Dst
    | BOE Dst Src Tgt      -- Bind Out Edge from Src to Tgt
    | BED Dst Reg Reg      -- Bind an EDge between Regs in either direction
    | BON Dst Dst Src      -- Bind Out-edge and Node by following one of Src's outgoing edges
    | BIN Dst Dst Tgt      -- Bind In-edge and Node by following one of Tgt's incoming edges
    | BEN Dst Dst Reg      -- Bind Edge and Node in either direction from Reg
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

    -- These are new instructions to support the graph oracle ("ORIcL"?)
--    | NEED Int Spc         -- assert that a rule requires there to be Int nodes in Spc
--    | NDFT Int Feature     -- assert that a rule requires there to be Int graph Features
--    | CHFT Int Feature     -- increase or decrease the number of available Feature by Int
--    | STFT Int Feature     -- set the number of available Feature to be Int
    deriving (Show, Eq)


compileProg :: OilrConfig -> [OilrIR] -> (OilrConfig, Prog)
compileProg cfg ir = foldr compile (cfg, []) ir

compile :: OilrIR -> (OilrConfig, Prog) -> (OilrConfig, Prog)
compile (IRProc name e)  (cfg, is) = (cfg,  ((mangle name, ([], defn, [RET])):is) )
    where defn = ProcBody (compileExpr t e)
          t = length is * 1000
compile (IRRule name es) (cfg, is) = (cfg', defn:is)
    where (defn, cfg') = compileRule (mangle name) cfg es

-- Compile a rule definition
compileRule :: String -> OilrConfig -> OilrRule -> (Definition, OilrConfig)
compileRule name cfg ms = (defn, cfg')
    where defn = ( name, ([REGS (length regs)]
                       , RuleBody (reverse lhs) (SUC:reverse rhs)
                       , [UBN (length regs), RET]) )
          (cfg', regs, RuleBody lhs rhs) = foldr compileMod (cfg, [], RuleBody [] []) $ reverse ms


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


compileMod :: (OilrMod OilrElem) -> (OilrConfig, Mapping Id Int, DefBody) -> (OilrConfig, Mapping Id Int, DefBody)
compileMod (Create x) (cfg, regs, body) = case x of
    (IRNode id _ _ _     ) -> ( cfg, (id,r):regs, growRule body [] [ABN r] )
    (IREdge id _ _ _ s t ) -> ( cfg, (id,r):regs, growRule body [] [abe regs r s t] )
    where r = length regs
compileMod (Delete x) (cfg, regs, body) = case x of
    (IRNode id _ _ _)      -> ( cfg', (id,r):regs, growRule body [BND r sid] [DBN r] )
    (IREdge id _ _ bi s t) -> ( cfg,  (id,r):regs, growRule body [bed regs r s t bi] [DBE r] )
    where r = length regs
          cfg' = makeSpc cfg (Delete x)
          sid = fst $ head $ searchSpaces cfg'
compileMod (Same x)   (cfg, regs, body) = case x of
    IRNode id _ _ _      -> (cfg', (id,r):regs, growRule body [BND r sid] [])
    IREdge id _ _ bi s t -> (cfg,  (id,r):regs, growRule body [bed regs r s t bi] [])
    where r = length regs
          cfg' = makeSpc cfg (Same x)
          sid = fst $ head $ searchSpaces cfg'
compileMod (Change left right) (cfg, regs, body) = case (left, right) of
    (IRNode id _ _ _     , IRNode _ _ _ _)
            -> (cfg', (id,r):regs, growRule body [BND r sid]         (diffs regs r left right) )
    (IREdge id _ _ bi s t, IREdge _ _ _ _ _ _)
            -> (cfg,  (id,r):regs, growRule body [bed regs r s t bi] (diffs regs r left right) )
    where r = length regs
          cfg' = makeSpc cfg $ Change left right
          sid = fst $ head $ searchSpaces cfg'
compileMod (Check (IRNot (IREdge id _ _ _ s t))) (cfg, regs, body) =
                            (cfg, regs, growRule body [nec regs s t] [])
-- compileMod x _ = error $ show x

growRule :: DefBody -> [Instr] -> [Instr] -> DefBody
growRule (RuleBody lhs rhs) lhsIns rhsIns = RuleBody lhs' rhs'
    where lhs'  = lhsIns  ++ lhs
          rhs'  = rhsIns  ++ rhs

diffs :: Mapping Id Int -> Reg -> OilrElem -> OilrElem -> [Instr]
diffs regs r (IRNode ib cb lb (Sig _ _ _ rb)) (IRNode ia ca la (Sig _ _ _ ra)) =
    -- TODO: root flag setting not detected!
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
bed regs r s t False = BOE r (definiteLookup s regs) (definiteLookup t regs)
bed regs r s t True  = BED r (definiteLookup s regs) (definiteLookup t regs)


abe :: Mapping Id Reg -> Reg -> Id -> Id -> Instr
abe regs r s t = ABE r (definiteLookup s regs) (definiteLookup t regs)

nec :: Mapping Id Reg -> Id -> Id -> Instr
nec regs s t = NEC (definiteLookup s regs) (definiteLookup t regs)

-- compileExpr :: (Int, [Instr]) -> OilrExpr -> (Int, [Instr])
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
compileSequence i es = concat [ compileExpr i' e | (e, i') <- zip es [i..] ]

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

