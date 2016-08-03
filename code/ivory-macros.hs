{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import qualified Ivory.Compile.C.CmdlineFrontend as C
import           Ivory.Language

printfU32 :: Def ('[IString, Uint32] ':-> ())
printfU32 = importProc "printf" "stdio.h"

puts :: Def ('[IString] ':-> ())
puts = importProc "puts" "stdio.h"

class CPrint a where
    cprint :: a -> Ivory eff ()

instance CPrint Uint32 where
    cprint = call_ printfU32 "%u\n"

instance CPrint IString where
    cprint = call_ puts

cmain :: Def ('[] ':-> Sint32)
cmain = proc "main" $ body $ do
    cprint (1 :: Uint32)
    cprint ("hello" :: IString)
    ret 0

ivoryMacros :: Module
ivoryMacros = package "ivory-macros" $ do
    incl cmain
    incl printfU32
    incl puts

main :: IO ()
main = C.compile [ivoryMacros] []
