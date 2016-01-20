module OILR3.Config where

import OILR3.IR

import Mapping

import Data.List

-- OilrConfig represents the global configuration of the OILR machine
-- for the current program.

data Flag = DisableOilr | DisableSearchPlan | OilrInstructions | RecursiveRules | EnableDebugging | EnableParanoidDebugging | EnableExecutionTrace | Compile32Bit | CompactLists deriving (Eq, Show)


data OilrConfig = OilrConfig { compilerFlags  :: [Flag]
                             , predicateRules :: [Id]   }


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

