module OILR4.Config where

import OILR4.IR

import GPSyntax  -- for colours
import Mapping

import Data.List

-- OilrConfig represents the global configuration of the OILR machine
-- for the current program.

type Ind = Int  -- An OILR index is just an integer

data Flag = DisableOilr | DisableSearchPlan | OilrInstructions | RecursiveRules | EnableDebugging | EnableParanoidDebugging | EnableExecutionTrace | Compile32Bit | CompactLists deriving (Eq, Show)

data OilrIndexBits = OilrIndexBits { bBits::Int
                                   , cBits::Int
                                   , oBits::Int
                                   , iBits::Int
                                   , lBits::Int
                                   , rBits::Int } deriving (Show, Eq)

indBits = OilrIndexBits 1 3 2 2 2 1



data OilrConfig = OilrConfig { compilerFlags  :: [Flag]
                             , predicateRules :: [String]
                             , searchSpaces   :: Mapping Int [Ind]}

colourIds :: Mapping Colour Int
colourIds = [ (Uncoloured, 0)
            , (Red       , 1)
            , (Blue      , 2)
            , (Green     , 3)
            , (Grey      , 4) ]
edgeColourIds :: Mapping Colour Int
edgeColourIds = [ (Uncoloured, 0) , (Dashed, 1) ]


configureOilrMachine :: [Flag] -> [OilrIR] -> OilrConfig
configureOilrMachine flags prog =
    OilrConfig { predicateRules = findPredicateRules prog 
               , compilerFlags  = flags
               , searchSpaces   = []}



findPredicateRules :: [OilrIR] -> [Id]
findPredicateRules prog = concatMap preds prog
    where preds (IRProc _ _) = []
          preds (IRRule id r) = case filter hasMods r of
                                    [] -> [id]
                                    _ -> []
          hasMods (Change _ _) = True
          hasMods (Create _) = True
          hasMods (Delete _) = True
          hasMods _ = False

