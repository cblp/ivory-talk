{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

import           Data.String
import qualified Ivory.Compile.C.CmdlineFrontend as C
import           Ivory.Language
import           System.Environment

puts :: Def ('[IString] ':-> ())
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
    msg:rest <- getArgs
    withArgs rest $ C.compile [hello msg] []