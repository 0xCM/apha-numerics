module Alpha.Numerics.Algebra.Series 
(
    Series(..), 
    series, summation, prodation
)
where

import Alpha.Numerics.Base
import Alpha.Numerics.Algebra.IndexedTerm
import qualified Data.List as List

newtype Series i t = Series (BinaryOperator t, IndexRange i, IndexedTerm i t) 
    
-- | Constructs a finite Series over a function
series::(Integral i, Nullary t) => BinaryOperator t ->  (i,i) -> Func i t -> Series i t
series f i t = Series (f, IndexRange i, term t)

summation::(Integral i,Nullary t, Additive t) => (i,i) -> Func i t -> Series i t
summation i t = series (+) i t

prodation::(Integral i,Nullary t, Multiplicative t) => (i,i) -> Func i t -> Series i t
prodation i t = series (*) i t


instance (Integral i, Nullary t)  => Computable (Series i t) where
    type Computation (Series i t) = t

    compute (Series (f, r, t)) = aggregation where
        expansion = (unwrap t) <$> discretize r
        aggregation = reduce f expansion

