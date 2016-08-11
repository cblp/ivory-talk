{-# LANGUAGE QuasiQuotes #-}

import           Language.C.Quote.C
import           Text.PrettyPrint.Mainland
-- import           Text.Show.Pretty

main :: IO ()
main = do
    let msg = "CoLaboratory: ruHaskell 2016"
    let code = [cunit|
        $esc:("#include <stdio.h>")
        int main() {
            puts($string:msg);
            return 0;
        }
    |]
    putDoc $ ppr code
    -- putStrLn $ ppShow code
