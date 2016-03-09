module OILR4.Config where

import OILR4.IR

import Mapping

import Data.List

-- OilrConfig represents the global configuration of the OILR machine
-- for the current program.

data Flag = DisableOilr | DisableSearchPlan | OilrInstructions | RecursiveRules | EnableDebugging | EnableParanoidDebugging | EnableExecutionTrace | Compile32Bit | CompactLists deriving (Eq, Show)

data OilrIndexBits = OilrIndexBits { bBits::Int
                                   , cBits::Int
                                   , oBits::Int
                                   , iBits::Int
                                   , lBits::Int
                                   , rBits::Int } deriving (Show, Eq)

indBits = OilrIndexBits 1 3 2 2 2 1



data OilrConfig = OilrConfig { compilerFlags  :: [Flag]
                             , predicateRules :: [String] }


configureOilrMachine :: [Flag] -> [OilrIR] -> OilrConfig
configureOilrMachine flags prog =
    OilrConfig { predicateRules = findPredicateRules prog 
               , compilerFlags  = flags }



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

