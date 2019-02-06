{-# LANGUAGE ExtendedDefaultRules #-}

module Alpha.Numerics.Algebra.Primality
(
    isPrime,
    primes,
    Factorization(..),
    factor'
)
where

import Alpha.Numerics.Base
import Control.Arrow
import qualified Data.List as List
import Prelude(divMod)
import qualified Alpha.Canonical.Common.Asci as Asci
default (Natural, Double, Text)

--Adapted from https://stackoverflow.com/questions/21276844/prime-factors-in-haskell    

newtype Factorization i = Factorization (i, [Exponential i Natural])
    deriving (Eq)

instance (Formattable i) => Formattable (Exponential i Natural) where
    format (Exponential (b,p)) = (format b) <> Asci.Caret <> (format p)

instance (Formattable i, Show i) => Formattable (Factorization i) where
    format (Factorization (n,exp)) = lhs <> Asci.Eq <> rhs where
        lhs = format n
        rhs = fmap format exp |> weave Asci.Plus |> append

instance (Formattable i,Show i) => Show(Factorization i) where
    show  = string . format

isPrime::(NaturalNumeric i, OrdNum i) => i -> Bool
isPrime n | n < 2 = False
isPrime n = all (\p -> n % p /= 0) . while ((<= n) . (^ 2)) $ primes

primes::(NaturalNumeric i, OrdNum i) => [i]
primes = 2 : List.filter (isPrime . fromIntegral)  [3..]

factor'::(NaturalNumeric i, OrdNum i) => i -> Factorization i
factor' n = iter n primes |> exponents |> fact  where

    fact exp = Factorization (n, exp)

    iter::(NaturalNumeric i, OrdNum i) => i -> [i] -> [i]
    iter n (p:_) | n < p^2 = [n | n > 1]
    iter n ps@(p:ps') =
        let (d, r) = n /% p
        in if r == 0 then p : iter d ps else iter n ps'

    exponents::(NaturalNumeric i, OrdNum i) => [i] -> [Exponential i Natural]
    exponents src = result where
        result = fmap (List.head &&& length)  (List.groupBy (==) src) |> fmap (\(b,p) -> Exponential (b,p)) 
        
unfactor'::(NaturalNumeric a) => Factorization a -> a
unfactor' (Factorization (_, exponentials)) = result where
    pairs = pair <$> exponentials
    pair (Exponential (b,p)) = (b,p)
    eval (b,p) = b^p
    factors = eval <$> pairs
    result = reduce one (*) factors

class Factorable a where
    factor::a -> Factorization a
    unfactor::Factorization a -> a
    
    
instance Factorable Natural where
    factor = factor'
    unfactor = unfactor'

instance Factorable Word where
    factor = factor'
    unfactor = unfactor'

instance Factorable Word8 where
    factor = factor'
    unfactor = unfactor'

instance Factorable Word16 where
    factor = factor'
    unfactor = unfactor'

instance Factorable Word32 where
    factor = factor'
    unfactor = unfactor'

instance Factorable Word64 where
    factor = factor'
    unfactor = unfactor'
                
    