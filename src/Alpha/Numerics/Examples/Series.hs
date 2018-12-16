{-# LANGUAGE ExtendedDefaultRules #-}

module Alpha.Numerics.Examples.Series where
import Alpha.Numerics


default(Int,Double,Text)    

instance Example "s-1" where
    example = do
        -- Sum the numbers from 1..100
        let s = summation (1,100) id
        let result1 = compute s
        let result2 = (100*101)/2
        print result1
        print result2
        print $ claimEqual @"summation (1,n) == (n*(n+1))/2" result1 result2
        
            
instance Example "s-2" where
    example = do
        let p = prodation (1,10) id 
        let a = compute p
        let b = factorial 10
        print a
        print b
        print $ claimEqual @"prodation (1,10) == factorial 10" a b

        