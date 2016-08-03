{-# LANGUAGE QuasiQuotes #-}

import           Language.C.Quote.C
import           System.Environment
import           Text.PrettyPrint.Mainland

main :: IO ()
main = do
    msg:_ <- getArgs
    putDoc $ ppr [cunit|
        $esc:("#include <stdio.h>")
        int main() {
            puts($string:msg);
        }
    |]
