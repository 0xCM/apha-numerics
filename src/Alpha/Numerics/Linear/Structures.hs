{-# LANGUAGE ExtendedDefaultRules #-}
module Alpha.Numerics.Linear.Structures
(
    submatrix

) where
import Data.Array.Repa(map, deepSeqArray, deepSeqArrays, fromFunction)
import Data.Array.Repa.Eval(now,fromList)
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Eval as Repa hiding(one)
import qualified Data.Array.Repa.Index as Repa
import Data.Array.Repa.Index(Z(..), (:.)(..), (:.))
import Data.Array.Repa(U,D)
import Control.Monad
import Control.Monad.ST.Strict    
import Alpha.Numerics.Base
import Alpha.Numerics.Linear.Repa

import qualified Data.Vector as V
import qualified Data.List as L

default(Int,Double)    
        
submatrix::forall r1 c1 r2 c2 m n a. 
    (NatPair m n, NatQuad r1 c1 r2 c2, Unbox a)  
        =>  Matrix U m n a -> Matrix D (r2 - r1) (c2 - c1) a
submatrix (Matrix arr) = Matrix $ subtable r arr
    where
        (m,n) = nat2 @m @n        
        r = region (nat2 @r1 @c1) (nat2 @r2 @c2)
    
instance (Unbox a) => Computable (Matrix U m n a) where
    type Computation (Matrix U m n a) = (Matrix U m n a)
    compute = id
    
instance (Unbox a) => Delayable (Matrix U m n a) where
    type Delayed (Matrix U m n a) = Matrix D m n a
    delay (Matrix m) = Matrix $ delay m

instance Delayable (Matrix D m n a) where
    type Delayed (Matrix D m n a) = Matrix D m n a
    delay = id
    
instance forall m n a. (NatPair m n, Nullary a) => Nullary (Matrix D m n a) where
    zero = Matrix $ unwrap $ tableD dim z where
        dim = nat2 @m @n  
        z = V.replicate (natmul @m @n) (zero @a)

instance forall m n a. (NatPair m n, Unital a) => Unital (Matrix D m n a) where
    one = Matrix $ unwrap $ tableD dim z where
        dim = nat2 @m @n  
        z = V.replicate (natmul @m @n) (one @a)

instance forall m n a r. (NatPair m n, Unbox a, Repa.Source r a) => Transposable (Matrix r m n a) where
    type Transposed (Matrix r m n a) = Matrix D n m a

    transpose (Matrix arr) = Matrix $ arr `deepSeqArray` do   
        backpermute new_extent swap arr
        where  
            swap (Z :. i :. j) = Z :. j :. i
            new_extent = swap (extent arr)
    {-# NOINLINE transpose #-}
                
instance forall m n p a. (NatTriple m n p, Numeric a, Unbox a) => HMultiplicative (Matrix U m n a) (Matrix U n p a) where
    type HProduct (Matrix U m n a) (Matrix U n p a) = Matrix D m p a    
    hmul m1 m2 = Matrix $ [arr, brr]  `deepSeqArrays` (runST $ do      
        trr  <- now $ unwrap $ compute $ transpose m2
        let (Z :. i  :. _)  = extent arr
        let (Z :. _   :. j) = extent brr
        return  $ fromFunction (Z :. i :. j)
                $ \ix -> accumulate $ zip (*) (row arr ix) (column trr ix))                    
        where 
            (Matrix arr, Matrix brr) = (m1, m2)
    {-# NOINLINE hmul #-}
    
instance forall m n a.(KnownNat m, KnownNat n, Unbox a) => IsList (Matrix U m n a) where
    type Item (Matrix U m n a) = a
    toList (Matrix m) = Repa.toList m
    fromList src  =  matrixU @m @n src

instance forall m n a.(KnownNat m, KnownNat n) => IsList (Matrix D m n a) where
    type Item (Matrix D m n a) = a
    toList (Matrix m) = Repa.toList m
    fromList src  =  matrixD @m @n src
            
instance forall n a. (KnownNat n, Unbox a) => HMultiplicative (Covector U n a) (Vector U n a) where
    type HProduct (Covector U n a) (Vector U n a) = a
    hmul = undefined