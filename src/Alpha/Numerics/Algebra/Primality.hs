{-# LANGUAGE ExtendedDefaultRules #-}

module Alpha.Numerics.Algebra.Primality
(
    isPrime,
    primes,
    UInt,
    Factorization(..),
)
where

import Alpha.Numerics.Base
import Control.Arrow
import qualified Data.List as List
import Prelude(divMod)
import qualified Alpha.Text.Asci as Asci
default (Natural, Double, Text)


--Adapted from https://stackoverflow.com/questions/21276844/prime-factors-in-haskell    



type UInt i = UnsignedIntegral i

newtype Factorization i = Factorization (i, [Exponential i i])
    deriving (Eq)

instance (Formattable i) => Formattable (Exponential i i) where
    format (Exponential (b,p)) = (format b) <> Asci.Caret <> (format p)

instance (Formattable i, Show i) => Formattable (Factorization i) where
    format (Factorization (n,exp)) = lhs <> Asci.Eq <> rhs where
        lhs = format n
        rhs = fmap format exp |> intersperse Asci.Plus |> splat

instance (Formattable i,Show i) => Show(Factorization i) where
    show  = string . format


type instance Factored Natural = Factorization Natural
type instance Factored Word = Factorization Word
type instance Factored Word8 = Factorization Word8
type instance Factored Word16 = Factorization Word16
type instance Factored Word32 = Factorization Word32
type instance Factored Word64 = Factorization Word64

isPrime::(UInt i) => i -> Bool
isPrime n | n < 2 = False
isPrime n = all (\p -> n % p /= 0) . while ((<= n) . (^ 2)) $ primes

primes::(UInt i) => [i]
primes = 2 : filter (isPrime . fromIntegral)  [3..]

factor'::(UInt i) => i -> Factorization i
factor' n = iter n primes |> exponents |> fact  where

    fact exp = Factorization (n, exp)

    iter::(UInt i) => i -> [i] -> [i]
    iter n (p:_) | n < p^2 = [n | n > 1]
    iter n ps@(p:ps') =
        let (d, r) = n /% p
        in if r == 0 then p : iter d ps else iter n ps'

    exponents::(UInt i) => [i] -> [Exponential i i]
    exponents src = result where
        result = fmap (head &&& length)  (group (==) src) |> fmap (\(b,p) -> exponential b p) 
        

instance Factorable Natural where
    factor = factor'
instance Factorable Word where
    factor = factor'
instance Factorable Word8 where
    factor = factor'
instance Factorable Word16 where
    factor = factor'
instance Factorable Word32 where
    factor = factor'
instance Factorable Word64 where
    factor = factor'
                
    