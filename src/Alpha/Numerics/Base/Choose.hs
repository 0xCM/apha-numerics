----------------------------------------------------------------
-- Adapted from the package exact-combinatorics by Gayle Romano
-- Copyright   :  Copyright (c) 2011--2015 wren gayle romano
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  Haskell98
--
-- Binomial coefficients (<http://oeis.org/A007318>), aka the count
-- of possible combinations. For negative inputs, all functions
-- return 0 (rather than throwing an exception or using 'Maybe').
----------------------------------------------------------------
module Alpha.Numerics.Base.Choose (choose) where
import Alpha.Numerics.Base
import Data.List(foldl',zipWith)
import Prelude(($!), asTypeOf)



{-
<http://mathworld.wolfram.com/BinomialCoefficient.html>

Some identities, but not really material for RULES:
    n `choose` 0     = 1
    n `choose` 1     = n
    n `choose` 2     = 2*n*(n-1)
    n `choose` k     = n `choose` (n-k) when 0<=k<=n
    n `choose` k     = (-1)^k * ((k-n-1) `choose` k)
    n `choose` (k+1) = (n `choose` k) * ((n-k) / (k+1))
    (n+1) `choose` k = (n `choose` k) * (n `choose` (k-1))
    n `choose` j     = ((n-1)`choose` j) + ((n-1)`choose`(j-1)) when 0<j<n

Regarding the prime factorization/carries thing, also cf:
    Kummer (1852);
    Graham et al. (1989), Exercise 5.36, p. 245;
    Ribenboim (1989);
    Vardi (1991), p. 68

To extend to negative arguments and to complex numbers, see (Kronenburg 2011):
    n `choose` k
        | k >= 0    = (-1)^k     * ((-n+k-1) `choose` k)
        | k <= n    = (-1)^(n-k) * ((-k-1) `choose` (n-k))
        | otherwise = 0

According to Grinstead&Snell, p.95, when using the naive implementation
if you alternatete the multiplications and divisions then all
intermediate values are integers and none of the intermediate values
exceeds the final value. This property is retained in the fast
implementation.
-}


-- TODO: give a version that returns the prime-power factorization as [(Int,Int)]


-- | Exact binomial coefficients. For a fast approximation see
-- @math-functions:Numeric.SpecFunctions.choose@ instead. The naive
-- definition of the binomial coefficients is:
--
-- > n `choose` k
-- >     | k < 0     = 0
-- >     | k > n     = 0
-- >     | otherwise = factorial n `div` (factorial k * factorial (n-k))
--
-- However, we use a fast implementation based on the prime-power
-- factorization of the result (Goetgheluck, 1987). Each time @n@
-- is larger than the previous calls, there will be some slowdown
-- as the prime numbers must be computed (though it is still much
-- faster than the naive implementation); however, subsequent calls
-- will be extremely fast, since we memoize the list of 'primes'.
-- Do note, however, that this will result in a space leak if you
-- call @choose@ for an extremely large @n@ and then don't need
-- that many primes in the future. Hopefully future versions will
-- correct this issue.
--
-- * P. Goetgheluck (1987)
--    /Computing Binomial Coefficients/,
--    American Mathematical Monthly, 94(4). pp.360--365.
--    <http://www.jstor.org/stable/2323099>,
--    <http://dl.acm.org/citation.cfm?id=26272>
--
choose :: (IntegralNumeric a, OrdNum a) => a -> a -> a
    -- The result type could be any (Num b) if desired.
{-# SPECIALIZE choose::Integer -> Integer -> Integer, Int -> Int -> Int #-}
n `choose` k_
    | n `seq` k_`seq` False = undefined
    | 0 < k_ && k_ < n =
        k `seq` nk `seq` sqrtN `seq`
            foldl'
                (\acc prime -> step acc (fromIntegral prime))
                1
                (while (fromIntegral n >=) primes)
        -- BUG: 'takeWhile' isn't a good producer, so we shouldn't
        -- just @map fromIntegral@. In newer GHC my patch will make
        -- it in for it to be a good producer (and a good consumer).
    | 0 <= k_ && k_ <= n = 1 -- N.B., @binomial_naive 0 0 == 1@
    | otherwise          = 0
    where
    k     = fromIntegral $! if k_ > n / 2 then n - k_ else k_
    nk    = n - k
    sqrtN = floor (sqrt (fromIntegral n) :: Double) `asTypeOf` n

    step acc prime
        | acc `seq` prime `seq` False = undefined
        | prime > nk         = acc * prime
        | prime > n / 2 = acc
        | prime > sqrtN      =
            if n `rem` prime < k `rem` prime
            then acc * prime
            else acc
        | otherwise = acc * go n k 0 1
        where
        go n' k' r p
            | n' `seq` k' `seq` r `seq` p `seq` False = undefined
            | n' <= 0   = p
            | n' `rem` prime < (k' `rem` prime) + r
                        = go (n' / prime) (k' / prime) 1 $! p * prime
            | otherwise = go (n' / prime) (k' / prime) 0 p

data Wheel = Wheel {-# UNPACK #-}!Int ![Int]


-- BUG: the CAF is nice for sharing, but what about when we want
-- fusion and to avoid sharing? Using Data.IntList seems to only
-- increase the overhead. I guess things aren't being memoized/freed
-- like they should...

-- | The prime numbers. Implemented with the algorithm in:
--
-- * Colin Runciman (1997)
--    /Lazy Wheel Sieves and Spirals of Primes/, Functional Pearl,
--    Journal of Functional Programming, 7(2). pp.219--225.
--    ISSN 0956-7968
--    <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.55.7096>
--
primes :: [Int]
primes = seive wheels primes primeSquares
    where
    primeSquares = [p*p | p <- primes]
    
    wheels = Wheel 1 [1] : zipWith nextSize wheels primes
        where
        nextSize (Wheel s ns) p =
            Wheel (s*p) [n' | o  <- [0,s..(p-1)*s]
                            , n  <- ns
                            , n' <- [n+o]
                            , n' `mod` p > 0 ]
    
    -- N.B., ps and qs must be lazy. Or else the circular program is _|_.
    seive (Wheel s ns : ws) ps qs =
        [ n' | o  <- s : [2*s,3*s..(head ps-1)*s]
                , n  <- ns
                , n' <- [n+o]
                , s <= 2 || noFactorIn ps qs n' ]
        ++ seive ws (tail ps) (tail qs)
        where
            noFactorIn :: [Int] -> [Int] -> Int -> Bool
            noFactorIn (p:ps) (q:qs) x =
                q > x || x `mod` p > 0 && noFactorIn ps qs x

----------------------------------------------------------------
----------------------------------------------------------- fin.
