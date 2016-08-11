{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

import           Data.String
import qualified Ivory.Compile.C.CmdlineFrontend as C
import           Ivory.Language

puts :: Def ('[IString] ':-> Sint32)
puts = importProc "puts" "stdio.h"

cmain :: String -> Def ('[] ':-> Sint32)
cmain msg = proc "main" $ body $ do
    call_ puts (fromString msg)
    ret 0

hello :: String -> Module
hello msg = package "ivory-hello" $ do
    incl puts
    incl (cmain msg)

main :: IO ()
main = do
    let msg = "CoLaboratory: ruHaskell 2016"
    C.compile [hello msg] []
