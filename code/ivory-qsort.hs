{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main (main) where

import           Data.Char
import           GHC.TypeLits
import qualified Ivory.Compile.C.CmdlineFrontend as C
import           Ivory.Language
import           Ivory.Language.Cast             (ivoryCast)
import           Ivory.Language.Syntax           (Expr (ExpSafeCast))
import           Ivory.Language.Type

type Size = Uint64  -- C size_t
type IInt = Sint32  -- C int
type AString n = 'Array n ('Stored Uint8)
type CompareProcSig s a = '[ConstRef s ('Stored a), ConstRef s ('Stored a)] ':-> IInt
type CompareProc    s a = Def (CompareProcSig s a)
-- type CompareProcPtr s a = Pro

_puts :: Def ('[Ptr s ('Stored IChar)] ':-> ())
_puts = importProc "puts" "stdio.h"

puts :: KnownNat n => Ref s (AString n) -> Ivory eff ()
puts array = call_ _puts (ivoryCast array)

type QsortType s = Def  ('[ Ptr s ('Stored ())  -- pointer to array
                          , Size                -- number of elements
                          , Size                -- size of an element
                          , Ptr s ('Stored ())  -- compare procedure
                          ]
                        ':-> ())
_qsort  :: QsortType s
_qsort = importProc "qsort" "stdlib.h"

qsortBy ::  forall s n a eff
        .   (KnownNat n, IvoryArea ('Stored a), IvoryType a)
        =>  CompareProc s a -> Ref s ('Array n ('Stored a)) -> Ivory eff ()
qsortBy compareProc array =
    call_ _qsort
          (ivoryCast array)
          (fromInteger $ natVal (Proxy :: Proxy n))
          (sizeOf (Proxy :: Proxy ('Stored a)))
          (ivoryCast' (procPtr compareProc))

ivoryCast' :: forall a b . (IvoryVar a, IvoryExpr b) => a -> b
ivoryCast' x = wrapExpr (ExpSafeCast ty (unwrapExpr x))
  where ty = ivoryType (Proxy :: Proxy a)

cmp_u8_rev :: CompareProc s Uint8
cmp_u8_rev = proc "cmp_u8_rev" $ \px py -> body $ do
    x <- deref px
    y <- deref py
    ret $ safeCast x - safeCast y

cmain :: Def ('[] ':-> Sint32)
cmain = proc "main" $ body $ do
    let exampleData = "CoLaboratory:ruHaskell 2016"
    exampleArray <- local
        (iarray $ map (ival . fromIntegral . ord) exampleData :: Init (AString 32))
    puts exampleArray
    qsortBy cmp_u8_rev exampleArray
    puts exampleArray
    ret 0

ivoryQsort :: Module
ivoryQsort = package "ivory-qsort" $ do
    incl _puts
    incl _qsort
    incl cmp_u8_rev
    incl cmain

main :: IO ()
main = C.compile [ivoryQsort] []
