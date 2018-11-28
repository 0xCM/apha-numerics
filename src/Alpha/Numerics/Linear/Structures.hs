{-# LANGUAGE ExtendedDefaultRules #-}
module Alpha.Numerics.Linear.Structures
(
    Matrix(..), matrix, row, col,
    Vector(..), vector,
    Covector(..), covector,
    matmul,
    DataTable(..), DataVector(..),

    rowslice, colslice,
    test

) where
import qualified Data.Array.Repa as Repa
import Data.Array.Repa(Array,Shape,U,D,Source,Slice,DIM1,DIM2)
import Data.Array.Repa(map, sumAllS, deepSeqArray, deepSeqArrays, extent,computeUnboxedS,fromFunction)
import Data.Array.Repa.Eval(computeS,computeP,Load,Target,now)
import Data.Array.Repa.Unsafe(unsafeBackpermute,unsafeSlice)
import qualified Data.Array.Repa.Index as Repa
import Data.Array.Repa.Index(Z(..), (:.)(..), (:.))
import qualified Data.Array.Repa.Algorithms.Matrix as RM
import Data.Vector.Unboxed(Unbox(..))
import qualified Data.Array.Repa.Algorithms.Matrix as M
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.ST.Strict
    
import Alpha.Numerics.Base
import qualified Alpha.Numerics.Linear.Computable as Comp
import Alpha.Numerics.Linear.Computable(Computable(..),CompVector, CompTable)

default(Int,Double)    

-- A rectangular array of data
type DataTable a = Array U DIM2 a

-- A vector of data
type DataVector a = Array U DIM1 a

-- Defines a matrix representation
newtype Matrix m n a = Matrix (DataTable a)
    deriving(Eq,Show)

-- Defines a representation for a square matrix
newtype SquareMatrix n a = SquareMatrix (Matrix n n a)
    deriving(Eq,Show)

-- Defines a vector as a column matrix (a matrix of dimension r x 1)
newtype Vector r a = Vector (DataVector a)
    deriving(Eq,Show)

-- Defines a covector as a row matrix (a matrix of dimension 1 x c)
newtype Covector c a = Covector (DataVector a)
    deriving(Eq,Show)

-- For the moment, invokes 'computeS'
compute::(Load r1 sh e, Target r2 e) => Array r1 sh e -> Array r2 sh e
compute = computeS

natpair::forall m n. (KnownNat m, KnownNat n) => (Int,Int)
natpair = (natVal (proxy @m) |> int, natVal (proxy @n) |> int)
        
-- | Constructs a matrix of unboxed cells      
matrix::forall m n a. (KnownNat m, KnownNat n, Unbox a) => [a] -> Matrix m n a
matrix cells = Matrix (Repa.fromListUnboxed s cells) where
    (m',n') = natpair @m @n    
    s = Z :. m' :. n'

unmatrix::Matrix m n a -> DataTable a
unmatrix (Matrix m) = m
    
-- | Constructs a vector with unboxed components
vector::forall r a. (KnownNat r, Unbox a) => [a] -> Vector r a
vector components = Vector (Repa.fromListUnboxed s components) where
    s = Z :. (natVal (proxy @r) |> int)

-- | Constructs a covector with unboxed components
covector::forall c a. (KnownNat c, Unbox a) => [a] -> Covector c a
covector components = Covector (Repa.fromListUnboxed s components) where
    s = Z :. (natVal (proxy @c) |> int)
    
-- | Selects and produces a designated row from a matrix
row::forall m n a. (KnownNat m, KnownNat n, Unbox a) => Computable m n a -> Int -> Covector n a
row m idx =  Covector $ compute $ map id content 
    where VectorComp (content) = (Comp.row m idx)

-- | Selects and produces a designated column from a matrix    
col::forall m n a. (KnownNat m, KnownNat n, Unbox a) => Computable m n a -> Int -> Vector m a
col m idx = Vector $ compute $ map id content 
    where VectorComp (content) = (Comp.col m idx)    

rowidx::DIM2 -> Int
rowidx = RM.row

colidx::DIM2 -> Int
colidx = RM.col

rowslice::CompTable a -> DIM2 -> CompVector a
rowslice arr r = unsafeSlice arr (Repa.Any :. (rowidx r) :. Repa.All)

colslice::CompTable a -> DIM2 -> CompVector a
colslice arr c = unsafeSlice arr (Repa.Any :. (colidx c) :. Repa.All)

--sumAllS :: (Shape sh, Source r a, Num a) => Array r sh a -> a
zip :: (Shape sh, Source r1 a, Source r2 b) => (a -> b -> c) -> Array r1 sh a -> Array r2 sh b -> Array D sh c
zip = Repa.zipWith

matmul::forall m n p a.(KnownNat m, KnownNat n, KnownNat p, Numeric a, Unbox a) =>  Matrix m n a -> Matrix n p a -> Matrix m p a
matmul m1 m2 
 = Matrix $ [arr, brr]  `deepSeqArrays` (runST $ do      
                trr  <- now $ unmatrix $ transpose m2
                let (Z :. i  :. _)  = extent arr
                let (Z :. _   :. j) = extent brr
                return $ compute 
                    $ fromFunction (Z :. i :. j)
                    $ (\ix -> sumAllS $ zip (*) (unsafeSlice arr (Repa.Any :. (rowidx ix) :. Repa.All)) (unsafeSlice trr (Repa.Any :. (colidx ix) :. Repa.All))
                                         
                        )
                    
                    )                    
    where 
        (Matrix arr) = m1
        (Matrix brr) = m2
{-# NOINLINE matmul #-}

instance forall m n a. (KnownNat m, KnownNat n, Unbox a) => Transposable (Matrix m n a) where
    type Transposed (Matrix m n a) = Matrix n m a

    transpose::Matrix m n a -> Matrix n m a
    transpose (Matrix arr)
        = Matrix $ arr `deepSeqArray`
        do   computeUnboxedS $ unsafeBackpermute new_extent swap arr
            where  
                swap (Z :. i :. j)      = Z :. j :. i
                new_extent             = swap (extent arr)
    {-# NOINLINE transpose #-}

{-
let m1 = matrix @2 @3 [2,4,6,8,10,12]
let m2 = matrix @3 @2 [1,3,5,7,9,11]
let result = m1 >*< m2
print result
-}
instance forall m n p a.(KnownNat m, KnownNat n, KnownNat p, Numeric a, Unbox a) => HMultiplicative (Matrix m n a) (Matrix n p a) where
    type HProduct (Matrix m n a) (Matrix n p a) = Matrix m p a
    
    hmul = matmul

instance forall n a. (KnownNat n, Unbox a) => HMultiplicative (Covector n a) (Vector n a) where
    type HProduct (Covector n a) (Vector n a) = a
    hmul = undefined


-- >> (matrix @3 @3 [1..9]) ! (2,2)
-- 5
instance forall m n a. (KnownNat m, KnownNat n, Unbox a) => Indexed (Matrix m n a) (Int,Int) where
    type Found (Matrix m n a) (Int,Int) = a
    
    lookup (Matrix m) (i,j) = m Repa.! (Z :. i-1 :. j-1)

    
test::Int    
test = result where
    mat33 = matrix @3 @3 [1..9]
    test =  mat33 ! (2,2) 
    result = test