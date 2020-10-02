module May.ServelessTemplate (loadTemplate)

import qualified Language.Haskell.TH as TH

fileText :: FilePath -> TH.Q TH.Exp
fileText fp = TH.LitE . TH.StringL <$> TH.runIO (readFile fp)
