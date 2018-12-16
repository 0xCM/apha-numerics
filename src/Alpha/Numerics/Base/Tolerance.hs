{-# LANGUAGE DataKinds #-}

module Alpha.Numerics.Base.Tolerance
(
    within
)
where
import Alpha


-- | Calculates whether the magnitude of the difference between two
-- numbers is less than a specified level of tolerance    
within::(Numeric a) => a -> a -> a -> Bool
within x y e = abs (x - y) < e

class (Numeric a) => Approximate a where
    almost::a -> a -> Bool

    (~=)::a -> a -> Bool
    (~=) = almost


class (Numeric a) => Tolerance (n::Nat) a where
    epsilon::a

instance (Numeric a) => Tolerance 0 a where
    epsilon = 0

instance (Numeric a) => Tolerance 1 a where
    epsilon = 1
    
-- instance (Numeric a) => Tolerance 6 a where
--     epsilon = num 4
        