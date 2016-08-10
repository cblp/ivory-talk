{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import           GHC.TypeLits
import qualified Ivory.Compile.C.CmdlineFrontend as C
import           Ivory.Language

type IInt = Sint32  -- C int

printf :: Def ('[IString, Uint32] ':-> ())
printf = importProc "printf" "stdio.h"

csum :: KnownNat n => Def ('[ConstRef s ('Array n ('Stored Uint32))] ':-> Uint32)
csum = proc "sum" $ \xs -> body $ do
    r <- local izero
    arrayMap $ \i -> do
        s <- deref r
        x <- deref $ xs ! i
        store r $ s + x
    ret =<< deref r

cmain :: Def ('[] ':-> IInt)
cmain = proc "main" $ body $ do
    exampleArray <- constRef <$>
        local
            (iarray $ map ival [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
            :: Init ('Array 10 ('Stored Uint32)))
    s <- call csum exampleArray
    call_ printf "%u\n" s
    ret 0

ivorySum :: Module
ivorySum = package "ivory-sum" $ do
    incl printf
    incl (csum :: Def ('[ConstRef s ('Array 10 ('Stored Uint32))] ':-> Uint32))
    incl cmain

main :: IO ()
main = C.compile [ivorySum] []
