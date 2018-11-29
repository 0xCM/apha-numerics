module Alpha.Numerics.Linear.Repa
(
    DataTable, DataVector, CompVector, CompTable,
    Computable(..),

    compute, rowidx,colidx,
    row, column, subtable,
    accumulate,
    extent, zipcomp, colcomp,rowcomp,
    slice,backpermute,
    Region(..), region
    
)
where
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Algorithms.Matrix as Repa
import qualified Data.Array.Repa.Unsafe as Repa

import Data.Array.Repa(Array,Shape,U,D,Source,Slice,DIM1,DIM2,DIM3,FullShape,SliceShape)
import Data.Array.Repa(map, sumAllS, deepSeqArray, deepSeqArrays, extent,computeUnboxedS,fromFunction)
import Data.Array.Repa.Eval(computeS,computeP,Load,Target,now)

import qualified Data.Array.Repa.Index as Repa
import Data.Array.Repa.Index(Z(..), (:.)(..), (:.))
import Data.Vector.Unboxed(Unbox(..))
import qualified Data.Array.Repa.Algorithms.Matrix as M
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.ST.Strict
    
import Alpha.Numerics.Base

-- A vector of data
type DataVector a = Array U DIM1 a

-- A rectangular array of data
type DataTable a = Array U DIM2 a

-- A 3-cube of data (actually, a parallelepiped)
type DataCube a = Array U DIM3 a

-- A vector of potential data
type CompVector a = Array D DIM1 a

-- A rectangular array of potential data
type CompTable a = Array D DIM2 a

-- A cube of potential data
type CompCube a = Array D DIM3 a

-- Represents a computation over a linear structure    
data Computable m n a 
    =   VectorComp (CompVector a)     
      | MatrixComp (CompTable a)     
    deriving(Eq)

newtype Region = Region (DIM2, DIM2)
    deriving(Eq, Show)

backpermute:: forall r sh1 sh2 e.( Shape sh1, Source r e) => sh2 -> (sh2 -> sh1) -> Array r  sh1 e -> Array D  sh2 e
backpermute = Repa.backpermute

slice::(Slice sl, Shape (FullShape sl), Source r e) => Array r (FullShape sl) e -> sl -> Array D (SliceShape sl) e
slice = Repa.unsafeSlice
{-# INLINE slice #-}

-- Extracts the row index from a rank-2 dimension
rowidx::DIM2 -> Int
rowidx = Repa.row

-- Extracts the column index from a rank-2 dimension
colidx::DIM2 -> Int
colidx = Repa.col

-- Defines a computable vector from a row
row::(Unbox a) => DataTable a -> DIM2 -> CompVector a
row arr r = slice arr (Repa.Any :. (rowidx r) :. Repa.All)

-- Defines a computable vector from a column
column::(Unbox a) => DataTable a -> DIM2 -> CompVector a
column arr c = slice arr (Repa.Any :. (colidx c) :. Repa.All)

subtable::(Unbox a) => Region ->  DataTable a -> CompTable a
subtable (Region (rc1 , rc2)) arr = Repa.extract rc1 rc2 arr 
    --where
    -- rc1 = Z :. r1 :. c1
    -- rc2 = Z :. r2 :. c2

-- Given two locations in a matrix, returns the dimensional 
-- range that identifies the data in a corresponding matrix
region::(Int,Int) -> (Int,Int) -> Region
region (r1,c1) (r2, c2) = Region ( Z :. r1 :. c1, Z :. (r2 - r1 + 1) :. (c2 - c1 + 1)) 

-- Executes an array computation
compute::(Load r1 sh e, Target r2 e) => Array r1 sh e -> Array r2 sh e
compute = Repa.computeS

zipcomp :: (Shape sh, Source r1 a, Source r2 b) => (a -> b -> c) -> Array r1 sh a -> Array r2 sh b -> Array D sh c
zipcomp = Repa.zipWith

-- | Defines a computation that, when actualized, produces a selected column        
colcomp::forall m n a. (KnownNat m, KnownNat n) => Computable m n a -> Int -> Computable m 1 a
colcomp (MatrixComp arr) ix = VectorComp $ Repa.slice arr (Repa.Any :. ix) where

-- | Defines a computation that, when actualized, produces a selected row    
rowcomp::forall m n a. (KnownNat m, KnownNat n) => Computable m n a -> Int -> Computable 1 n a
rowcomp (MatrixComp arr) ix = VectorComp $ Repa.slice arr (Repa.Any :. ix :. Repa.All)

-- | Sums the elements of an array
accumulate::(Shape sh, Source r a, Num a) => Array r sh a -> a
accumulate = Repa.sumAllS

instance (Source r a) => Indexed (Array r DIM1 a) Int where
    type Found (Array r DIM1 a) Int = a    
    lookup arr i = arr Repa.! (Z :. i)
    
instance (Source r a) => Indexed (Array r DIM2 a) (Int,Int) where
    type Found (Array r DIM2 a) (Int,Int) = a    
    lookup arr (i,j) = arr Repa.! (Z :. i :. j)
    
instance (Source r a) => Indexed (Array r DIM3 a) (Int,Int,Int) where
    type Found (Array r DIM3 a) (Int,Int,Int) = a    
    lookup arr (i,j,k) = arr Repa.! (Z :. i :. j :. k )
    