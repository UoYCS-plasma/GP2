module OILR3.EmbedFileContents where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

literally :: String -> Q Exp
literally = return . LitE . StringL

improperUseOfEmbed = error "Improper use of QuasiQuoter provided by OILR3/EmbedFileContents.hs"

lit :: QuasiQuoter
lit = QuasiQuoter { quoteExp  = literally
                  , quotePat  = improperUseOfEmbed
                  , quoteType = improperUseOfEmbed
                  , quoteDec  = improperUseOfEmbed }

litFile :: QuasiQuoter
litFile = quoteFile lit

