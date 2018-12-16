{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DataKinds #-}

module Alpha.Numerics.Examples.Primes where
import Alpha.Numerics
import qualified Alpha.Structures as Struct

default(Natural,Double,Text) 

instance Example "pr-1" where
    example = do
        let factors = factor 581
        print factors
