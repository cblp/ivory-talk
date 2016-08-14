{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import qualified Ivory.Compile.C.CmdlineFrontend as C
import           Ivory.Language

type IInt = Sint32  -- C int

printf :: Def ('[IString, Uint32] ':-> ())
printf = importProc "printf" "stdio.h"

cmain :: Def ('[] ':-> IInt)
cmain = proc "main" $ body $ do
    exampleArray <- local
        (iarray $ map ival [0, 1, 2, 3]
        :: Init ('Array 4 ('Stored Uint32)))
    s <- deref (exampleArray ! 4)
    call_ printf "%u\n" s
    ret 0

ivoryQuirk :: Module
ivoryQuirk = package "ivory-quirk" $ do
    incl printf
    incl cmain

main :: IO ()
main = C.compile [ivoryQuirk] []
