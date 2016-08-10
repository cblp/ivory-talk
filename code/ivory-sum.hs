{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main (main) where

import           Control.Applicative             (liftA2)
import           GHC.TypeLits
import qualified Ivory.Compile.C.CmdlineFrontend as C
import           Ivory.Language

type IInt = Sint32  -- C int

printf :: Def ('[IString, Uint32] ':-> ())
printf = importProc "printf" "stdio.h"

(+=) :: (IvoryStore a, Num a) => Ref s1 ('Stored a) -> ConstRef s2 ('Stored a) -> Ivory eff ()
r += x = store r =<< liftA2 (+) (deref r) (deref x)

csum :: (KnownNat n, IvoryStore a, IvoryZeroVal a, Num a)
     => Def ('[ConstRef s ('Array n ('Stored a))] ':-> a)
csum = proc "sum" $ \xs -> body $ do
    acc <- local izero
    arrayMap $ \i ->
        acc += (xs ! i)
    ret =<< deref acc

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
