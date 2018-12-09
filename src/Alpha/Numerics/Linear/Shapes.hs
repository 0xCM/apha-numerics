module Alpha.Numerics.Linear.Shapes
(
    Region(..),
    DimNum(..), DimIx(..),
    rowidx, colidx, matidx, 
    rownum, colnum,  rowdef,coldef,region,natregion,
)
where
import Alpha.Numerics.Base
import Data.Array.Repa(DIM1,DIM2)    
import Data.Array.Repa.Index(Z(..), (:.)(..), (:.))
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Algorithms.Matrix as Repa

-- | Specifies a rectangular subset of a 2-d array
newtype Region = Region (DIM2, DIM2)
    deriving(Eq, Generic, Show)
instance Newtype (Region)    

-- A dimension *number* (1-based) as opposed to an *index* (0-based)
newtype DimNum = DimNum Int
    deriving(Eq,Generic,Show,Num,Ord,ToInt)
instance Newtype (DimNum)

-- A dimension *index* (0-based) as opposed to an *number* (1-based)
newtype DimIx = DimIx Int
    deriving(Eq,Generic,Show,Num,Ord,ToInt)
instance Newtype (DimIx)


rowdef i = Repa.Any :. i :. Repa.All

coldef i = Repa.Any :. i 

-- Extracts the row index from a rank-2 dimension
rowidx::DIM2 -> DimIx
rowidx = DimIx . Repa.row 
{-# INLINE rowidx #-}

-- Extracts the column index from a rank-2 dimension
colidx::DIM2 -> DimIx
colidx = DimIx . Repa.col
{-# INLINE colidx #-}

-- | Defines an  pair of indexes that identifies an element in a matrix
matidx::DIM2 -> (DimIx,DimIx)
matidx d = (rowidx d, colidx d)
{-# INLINE matidx #-}

rownum::DIM2 -> DimNum
rownum =  DimNum . ((+) 1) . Repa.row
{-# INLINE rownum #-}

colnum::DIM2 -> DimNum
colnum = DimNum . ((+) 1) . Repa.col
{-# INLINE colnum #-}

entrynum::DIM2 -> (DimNum,DimNum)
entrynum d = (colnum d, rownum d)
{-# INLINE entrynum #-}


-- Given two locations in a matrix (left,top) and (right, bottom), 
-- calculates the dimensional range that identifies the data in 
-- a corresponding 2D array
region::(Int,Int) -> (Int,Int) -> Region
region (r1,c1) (r2, c2) 
    = Region ( Z :. r1 :. c1, Z :. (r2 - r1 + 1) :. (c2 - c1 + 1)) 

natregion::forall r1 c1 r2 c2. KnownNatQuad r1 c1 r2 c2 => Region
natregion = region (nat2 @r1 @c1) (nat2 @r2 @c2) where
    
instance Dimensional Int where
    type Dimension Int = Z :. Int
    dimension i = Z :. i
    {-# INLINE dimension #-}

instance Dimensional (Int,Int) where
    type Dimension (Int,Int) = Z :. Int :. Int
    dimension (i,j) = Z :. i :. j
    {-# INLINE dimension #-}
