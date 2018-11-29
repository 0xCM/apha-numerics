{-# LANGUAGE ExtendedDefaultRules #-}
module Alpha.Numerics.Linear.Structures
(
    Matrix(..), matrix, submatrix,
    Vector(..), vector,
    Covector(..), covector,
    DataTable(..), 
    DataVector(..),
    
    test

) where
import qualified Data.Array.Repa as Repa
import Data.Array.Repa(map, deepSeqArray, deepSeqArrays, fromFunction)
import Data.Array.Repa.Eval(computeS,computeP,Load,Target,now,fromList)
import qualified Data.Array.Repa.Eval as Repa
import qualified Data.Array.Repa.Index as Repa
import Data.Array.Repa.Index(Z(..), (:.)(..), (:.))
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.ST.Strict    
import Alpha.Numerics.Base
import Alpha.Numerics.Linear.Repa

default(Int,Double)    

-- Defines a matrix representation
newtype Matrix m n a = Matrix (DataTable a)
    deriving(Show)

-- Defines a representation for a square matrix
newtype SquareMatrix n a = SquareMatrix (Matrix n n a)
    deriving(Eq,Show)

-- Defines a vector as a column matrix (a matrix of dimension r x 1)
newtype Vector r a = Vector (DataVector a)
    deriving(Show)

-- Defines a covector as a row matrix (a matrix of dimension 1 x c)
newtype Covector c a = Covector (DataVector a)
    deriving(Show)
    
-- | Constructs a matrix of unboxed cells      
matrix::forall m n a. (NatPair m n, Unbox a) => [a] -> Matrix m n a
matrix cells = Matrix (Repa.fromListUnboxed s cells) where
    (m',n') = nat2 @m @n    
    s = Z :. m' :. n'

-- | Constructs a vector with unboxed components
vector::forall r a. (KnownNat r, Unbox a) => [a] -> Vector r a
vector components = Vector (Repa.fromList s components) where
    s = Z :. (natVal (proxy @r) |> int)

-- | Constructs a covector with unboxed components
covector::forall c a. (KnownNat c, Unbox a) => [a] -> Covector c a
covector components = Covector (Repa.fromList s components) where
    s = Z :. (natVal (proxy @c) |> int)

-- | Extracts the underlying 'DataTable' from a matrix    
datatable::Matrix m n a -> DataTable a
datatable (Matrix m) = m

{-
    let m = matrix @5 @5 [1..25]
    let sm = submatrix @0 @0 @2 @2 m
    print sm
-}
submatrix::forall r1 c1 r2 c2 m n a m' n'. 
    (NatQuad m n m' n', NatQuad r1 c1 r2 c2, 
     m' ~ (r2 - r1), n' ~ (c2 - c1),
     Unbox a)  =>  Matrix m n a -> Matrix m' n' a
submatrix (Matrix table) = Matrix $ compute $ subtable r table
    where
        (m,n) = nat2 @m @n        
        r = region (nat2 @r1 @c1) (nat2 @r2 @c2)

instance (Unbox a, Eq a) =>  Eq (Covector c a) where
    (Covector cv1) == (Covector cv2) = Repa.equalsS cv1 cv2

instance (Unbox a, Eq a) =>  Eq (Matrix m n a) where
    (Matrix m2) == (Matrix m1) = Repa.equalsS m1 m2

instance (Unbox a, Eq a) =>  Eq (Vector c a) where
    (Vector v1) == (Vector v2) = Repa.equalsS v1 v2

{- Example 
    let m = matrix @3 @1 [1,2,3]
    print m
    let mt = transpose m
    print mt
-}
instance forall m n a. (NatPair m n, Unbox a) => Transposable (Matrix m n a) where
    type Transposed (Matrix m n a) = Matrix n m a

    transpose::Matrix m n a -> Matrix n m a
    transpose (Matrix arr)
        = Matrix $ arr `deepSeqArray` do   
            compute $ backpermute new_extent swap arr
                where  
                    swap (Z :. i :. j) = Z :. j :. i
                    new_extent = swap (extent arr)
    {-# NOINLINE transpose #-}

{-
let m1 = matrix @2 @3 [2,4,6,8,10,12]
let m2 = matrix @3 @2 [1,3,5,7,9,11]
let result = m1 >*< m2
print result
-}
instance forall m n p a. (NatTriple m n p, Numeric a, Unbox a) => HMultiplicative (Matrix m n a) (Matrix n p a) where
    type HProduct (Matrix m n a) (Matrix n p a) = Matrix m p a
    
    hmul m1 m2 
        = Matrix $ [arr, brr]  `deepSeqArrays` (runST $ do      
                        trr  <- now $ datatable $ transpose m2
                        let (Z :. i  :. _)  = extent arr
                        let (Z :. _   :. j) = extent brr
                        return $ compute 
                            $ fromFunction (Z :. i :. j)
                            $ \ix -> accumulate $ zipcomp (*) (row arr ix) (column trr ix)                                                                                 
                            )                    
            where 
                (Matrix arr, Matrix brr) = (m1, m2)
    {-# NOINLINE hmul #-}



instance forall m n a.(KnownNat m, KnownNat n, Unbox a) => IsList (Matrix m n a) where
    type Item (Matrix m n a) = a
    toList (Matrix m) = Repa.toList m
    fromList  = matrix @m @n
    
instance forall n a. (KnownNat n, Unbox a) => HMultiplicative (Covector n a) (Vector n a) where
    type HProduct (Covector n a) (Vector n a) = a
    hmul = undefined

-- >> (matrix @3 @3 [1..9]) ! (2,2)
-- 5
instance forall m n a. (NatPair m n, Unbox a) => Indexed (Matrix m n a) (Int,Int) where
    type Found (Matrix m n a) (Int,Int) = a    
    lookup (Matrix m) (i,j) = m Repa.! (Z :. i :. j)
    
test::Int    
test = result where
    mat33 = matrix @3 @3 [1..9]
    test =  mat33 ! (2,2) 
    result = test