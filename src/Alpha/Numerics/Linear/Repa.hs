{-# LANGUAGE UndecidableInstances #-}
module Alpha.Numerics.Linear.Repa
(
    DataTable(..),
    Matrix(..),
    Vector(..),
    Covector(..),
    Region(..),
    rowidx,colidx,
    row, column, subtable,
    accumulate,
    extent, zip, 
    slice,backpermute, 
    region,
    
    vectorD, vectorU,
    covectorD, covectorU,
    matrixD, matrixU,
    arrayU,
    tableD
    
)
where
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Algorithms.Matrix as Repa
import qualified Data.Array.Repa.Unsafe as Repa
import qualified Data.Array.Repa.Eval as Repa
import qualified Data.Array.Repa.Eval.Gang as Repa

import Data.Array.Repa(Array,Shape,U,D,Source,Slice,DIM1,DIM2,DIM3,FullShape,SliceShape)
import Data.Array.Repa(map, sumAllS, deepSeqArray, deepSeqArrays, extent,computeUnboxedS,fromFunction,fromListUnboxed)
import Data.Array.Repa.Eval(computeS,computeP,Load,Target,now)

import qualified Data.Array.Repa.Index as Repa
import Data.Array.Repa.Index(Z(..), (:.)(..), (:.))
import Data.Vector.Unboxed(Unbox(..))
import qualified Data.Array.Repa.Algorithms.Matrix as M
import qualified Data.Vector as V
import qualified Data.List as L
import Control.Monad
import Control.Monad.ST.Strict
    
import Alpha.Numerics.Base

newtype DataTable r a = DataTable (Array r DIM2 a)
    deriving (Eq, Generic)

newtype Matrix r m n a = Matrix (Array r DIM2 a)
    deriving (Eq, Generic)

newtype Covector r n a = Covector (Matrix r 1 n a)
    deriving (Eq, Generic)

newtype Vector r m a = Vector (Matrix r m 1 a)
    deriving (Eq, Generic)

-- | Specifies a rectangular subset of a 2-d array
newtype Region = Region (DIM2, DIM2)
    deriving(Eq, Show)
    
backpermute:: forall r s1 s2 a.(Shape s1, Source r a) => s2 -> (s2 -> s1) -> Array r  s1 a -> Array D s2 a
backpermute = Repa.backpermute

slice::(Slice s, Shape (FullShape s), Source r a) => Array r (FullShape s) a -> s -> Array D (SliceShape s) a
slice = Repa.unsafeSlice
{-# INLINE slice #-}

-- Extracts the row index from a rank-2 dimension
rowidx::DIM2 -> Int
rowidx = Repa.row
{-# INLINE rowidx #-}

-- Extracts the column index from a rank-2 dimension
colidx::DIM2 -> Int
colidx = Repa.col
{-# INLINE colidx #-}

-- Given two locations in a matrix (left,top) and (right, bottom), 
-- calculates the dimensional range that identifies the data in 
-- a corresponding 2D array
region::(Int,Int) -> (Int,Int) -> Region
region (r1,c1) (r2, c2) 
    = Region ( Z :. r1 :. c1, Z :. (r2 - r1 + 1) :. (c2 - c1 + 1)) 

-- Defines a computable vector from a row
row::(Unbox a) => Array U DIM2 a -> DIM2 -> Array D DIM1 a
row arr  r = slice arr (Repa.Any :. (rowidx r) :. Repa.All)

-- Defines a computable vector from a column
column::(Unbox a) => Array U DIM2 a -> DIM2 -> Array D DIM1 a
column arr c = slice arr (Repa.Any :. (colidx c) :. Repa.All)

subtable::(Source r a) => Region ->  Array r DIM2 a -> Array D DIM2 a
subtable (Region (rc1 , rc2)) arr =  Repa.extract rc1 rc2 arr 

zip::(Shape s, Source r1 a, Source r2 b) => (a -> b -> c) -> Array r1 s a -> Array r2 s b -> Array D s c
zip f a1 a2 = Repa.zipWith f a1 a2

-- | Produces an unboxed array
arrayU::(Shape s, Unbox a) => s -> [a] -> Array U s a
arrayU = Repa.fromListUnboxed
{-# INLINE arrayU #-}

matrixU::forall m n a. (NatPair m n, Unbox a) => [a] -> Matrix U m n a
matrixU src = Matrix $ Repa.fromListUnboxed dim src
    where
        (r,c) = nat2 @m @n
        dim = Z :. r :. c

{-# INLINE matrixU #-}


matrixD::forall m n a. (NatPair m n) => [a] -> Matrix D m n a
matrixD src = Matrix $ Repa.fromFunction dim (\i -> v V.! (rowidx i) ) 
    where
        (r,c) = nat2 @m @n
        dim = Z :. r :. c
        v = V.fromList src
{-# INLINE matrixD #-}

-- | Produces a delayed array
tableD::(Int,Int) -> V.Vector a -> DataTable D a
tableD (r,c) src =  DataTable $ Repa.fromFunction dim (\i -> src V.! (rowidx i) ) 
    where dim = Z :. r :. c
{-# INLINE tableD #-}

-- | Constructs a vector with unboxed components
vectorU::forall r a. (KnownNat r, Unbox a) => [a] -> Vector U r a
vectorU components = Vector (matrixU @r components) where
    s = Z :. (natVal (proxy @r) |> int)

vectorD::forall r a. (KnownNat r, Unbox a) => [a] -> Vector D r a
vectorD components = Vector $ delay $ matrixU @r components where
    s = Z :. (natVal (proxy @r) |> int)
    
-- | Constructs a covector with unboxed components
covectorU::forall c a. (KnownNat c, Unbox a) => [a] -> Covector U c a
covectorU components = Covector $ matrixU @1 @c components where
    
covectorD::forall c a. (KnownNat c) => [a] -> Covector D c a
covectorD components = Covector $ matrixD @1 @c components where
    s = Z :. (natVal (proxy @c) |> int)

instance (Shape s, Source r a) => Delayable (Array r s a)  where
    type Delayed (Array r s a) = Array D s a
    delay = Repa.delay

instance (Source r a) => Delayable (Matrix r m n a)  where
    type Delayed (Matrix r m n a) = Matrix D m n a
    delay (Matrix arr) = Matrix $ Repa.delay arr
    
-- | Sums the elements of an array
instance (Shape sh, Source r a, Num a) => Accumulator (Array r sh a) where
    type Accumulation (Array r sh a) = a
    accumulate = Repa.sumAllS

instance (Source r a, Num a) => Accumulator (Matrix r m n a) where
    type Accumulation (Matrix r m n a) = a
    accumulate (Matrix arr) = Repa.sumAllS arr
    
instance (Shape sh, Source r a, Negatable a) => Negatable (Array r sh a) where 
    type Negated (Array r sh a) = Array D sh (Negated a)
    negate arr = Repa.map negate arr

instance (Source r a, Negatable a) => Negatable (Matrix r m n a) where 
    type Negated (Matrix r m n a) = Matrix D m n (Negated a)
    negate (Matrix arr) = Matrix $ Repa.map negate arr
    
instance (Load r s a, Target U a) => Computable (Array r s a) where
    type Computation (Array r s a) = Array U s a
    compute = Repa.computeS

instance (Load r DIM2 a, Target U a) => Computable (Matrix r m n a) where
    type Computation (Matrix r m n a) = Matrix U m n a
    compute (Matrix arr) = Matrix $ Repa.computeS arr
        
instance (Source r a) => Indexed (Array r DIM1 a) Int where
    type Found (Array r DIM1 a) Int = a    
    lookup arr i = arr Repa.! (Z :. i)
    
instance (Source r a) => Indexed (Array r DIM2 a) (Int,Int) where
    type Found (Array r DIM2 a) (Int,Int) = a    
    lookup arr (i,j) = arr Repa.! (Z :. i :. j)

instance (Source r a) => Indexed (Matrix r m n a) (Int,Int) where
    type Found (Matrix r m n a) (Int,Int) = a    
    lookup (Matrix arr) (i,j) = arr Repa.! (Z :. i :. j)
        

instance forall m n a. (Show a, Unbox a) => Show (Matrix U m n a) where
    show(Matrix arr) = show arr

instance Newtype (DataTable r a)    
instance Newtype (Matrix r m n a)
instance Newtype (Covector r n a)    
instance Newtype (Vector r m a)        
    