{-# LANGUAGE QuasiQuotes #-}

import           Language.C.Quote.C
import           System.Environment
import           Text.PrettyPrint.Mainland
-- import           Text.Show.Pretty

main :: IO ()
main = do
    msg:_ <- getArgs
    let code = [cunit|
        $esc:("#include <stdio.h>")
        int main() {
            puts($string:msg);
        }
    |]
    putDoc $ ppr code
    -- putStrLn $ ppShow code

-- [ EscDef "#include <stdio.h>"
-- , FuncDef
--     (OldFunc
--        (DeclSpec [] [] (Tint Nothing))
--        (Id "main")
--        (DeclRoot)
--        []
--        Nothing
--        [ BlockStm
--            (Exp
--               (Just
--                  (FnCall
--                     (Var (Id "puts"))
--                     [ Const
--                         ( StringConst ["\"CoLaboratory:ruHaskell 2016\""]
--                           "CoLaboratory:ruHaskell 2016" ) ] ) ) ) ] ) ]
