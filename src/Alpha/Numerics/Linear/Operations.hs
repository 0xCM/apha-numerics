-----------------------------------------------------------------------------
-- | Core linear algebra operations
-- Copyright   :  (c) Lennart Augustsson
-- License     :  BSD3, per original source
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Numerics.Linear.Operations
(
    Transposable(..),
    dot, vector, covector,
    row, rows,
    col, cols,
    cross, submatrix, table, matrixF,matrix,
    kdelta,
    vtable


) where
import Control.Monad.ST.Strict    
import qualified Data.Vector as V
import Alpha.Numerics.Base
import Alpha.Numerics.Linear.Adapters
import Alpha.Numerics.Linear.Shapes
import Alpha.Structures.Linear

import Data.Array.Repa.Eval(Load(..))
import Data.Array.Repa.Index(Z(..), (:.)(..), (:.))
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Eval as Repa hiding(one)
import qualified Data.Array.Repa.Index as Repa

default(Int,Double)    

type instance Element (Matrix r m n a) = a
type instance Element (DataTable r a) = a
type instance Element (ElementFunc a) = a

type instance Multiplied (Matrix r1 m n a) (Matrix r2 n p a) = MatrixComp m p a    
type instance Multiplied (Covector r n a) (Vector r n a) = a
type instance Summed (Matrix r1 n m a) (Matrix r2 n m a) = (MatrixComp n m a)
type instance Multiplied (DataSlice r a) (DataSlice r a) = a    


class Transposable a where
    type Transposed a

    transpose::a -> Transposed a

dot::(ArraySource r a, Ring a) => (DataSlice r a, DataSlice r a) -> a
dot (v1, v2) = v1 >*< v2

matrix::forall m n a . (KnownNatPair m n, Unbox a) => [a] -> MatrixComp m n a
matrix v = Matrix $ Repa.delay $ Repa.fromListUnboxed dim v
    where
        (r,c) = nat2 @m @n
        dim = Z :. r :. c                        
{-# INLINE matrix #-}

-- | Produces a matrix by assigning each element to the result of a supplied function
matrixF::forall m n a .(KnownNatPair m n) => ((DimIx,DimIx) -> a) -> MatrixComp m n a
matrixF f = Matrix $ Repa.fromFunction dim (\s -> f (matidx s) ) 
    where
        (r,c) = nat2 @m @n
        dim = Z :. r :. c                        
{-# INLINE matrixF #-}

vector::forall r a. (KnownNat r, Unbox a) => [a] -> VectorComp r a
vector components = Vector $ compute $ matrix @r components where    
    s = Z :. nat @r
{-# INLINE vector #-}

-- | Constructs a covector computation    
covector::forall c a. (KnownNat c, Unbox a) => [a] -> CovectorComp c a
covector components = Covector $ matrix @1 @c components where
    s = Z :. nat @c
{-# INLINE covector #-}

-- | Selects an identified row of data from a matrix    
row::forall r m n a. (KnownNatPair m n, ArraySource r a) => Int -> Matrix r m n a -> SliceComp a
row i (Matrix arr) =  DataSlice sl where    
    sl = Repa.slice arr (rowdef i)
{-# INLINE row #-}

rows::forall r m n a. (KnownNatPair m n, ArraySource r a) => Matrix r m n a -> [SliceComp a]
rows m = [row i m | i <- [0..(nat @m) - 1]] where
{-# INLINE rows #-}    

cols::forall r m n a. (KnownNatPair m n, ArraySource r a) => Matrix r m n a -> [SliceComp a]
cols m = [col i m | i <- [0..(nat @n) - 1]] where
{-# INLINE cols #-}    

-- | Selects an identified column of data from a matrix    
col::forall r m n a. (KnownNatPair m n, ArraySource r a) => Int -> Matrix r m n a -> SliceComp a
col i (Matrix arr) =  DataSlice sl where    
    sl = Repa.slice arr (coldef i)
{-# INLINE col #-}

--- | Selects an identified row and column of data from a matrix
cross::forall r1 r2 m n p q a. (KnownNatQuad m n p q, ArraySource r1 a, ArraySource r2 a) 
    => (Int,Int) -> Matrix r1 m n a -> Matrix r2 p q a -> (SliceComp a, SliceComp a)
cross (i,j) m1 m2 = (row i m1, col j m2)
{-# INLINE cross #-}

-- | Extracts a submatrix from a source matrix    
submatrix::forall r1 c1 r2 c2 r m n a. (KnownNatPair m n, KnownNatQuad r1 c1 r2 c2, ArraySource r a)  
        =>  Matrix r m n a -> MatrixComp (r2 - r1) (c2 - c1) a
submatrix (Matrix arr) = Matrix $ subtable r arr
    where
        (m,n) = nat2 @m @n        
        r = region (nat2 @r1 @c1) (nat2 @r2 @c2)
{-# INLINE submatrix #-}

-- | Produces a table computation from a source vector
vtable::(Int,Int) -> V.Vector a -> TableComp a
vtable (r,c) src =  DataTable $ Repa.fromFunction dim (\i -> src ! int (rowidx i) ) 
    where dim = Z :. r :. c
{-# INLINE vtable #-}

kdelta::(Nullary a, Unital a) => (DimIx, DimIx) -> a
kdelta (i, j) = ifelse (i==j) one zero

rowmul::forall r m n a. (KnownNatPair m n, Multiplicative a, ArraySource r a) 
    => DimIx ->  a -> Matrix r m n a -> MatrixComp m n a
rowmul = undefined

instance MatrixSource (ElementFunc a) where
    type MatrixElement (ElementFunc a) = a
    matrix'::forall m n .(KnownNatPair m n) => ElementFunc a -> MatrixComp m n a    
    matrix' f = Matrix $ Repa.fromFunction dim (\s -> f (matidx s) ) 
        where
            (r,c) = nat2 @m @n
            dim = Z :. r :. c                        
    {-# INLINE matrix' #-}

instance (Unbox a) => MatrixSource [a] where    
    type MatrixElement [a] = a
    matrix'::forall m n. (KnownNatPair m n) => [a] -> MatrixComp m n a    
    matrix' v = Matrix $ Repa.delay $ Repa.fromListUnboxed dim v
        where
            (r,c) = nat2 @m @n
            dim = Z :. r :. c                        
    {-# INLINE matrix' #-}

instance forall r m n a b.(ArraySource r a) => IMappable (Matrix r m n a) a b  where 
    type MapIndex (Matrix r m n a) a b = (Int,Int)
    type IMapped (Matrix r m n a) a b = MatrixComp m n b
    mapi f (Matrix arr) = Matrix $ mapi (f . f') arr
        where 
            f'::(Repa.DIM2,a) -> ((Int,Int),a)
            f'(Z :. i :. j, a) = ((i,j),a)
    {-# INLINE mapi #-}                    

instance forall r m n a b.(ArraySource r a) => Mappable (Matrix r m n a) a b  where 
    type Mapped (Matrix r m n a) a b = MatrixComp m n b
    map f (Matrix arr) = Matrix $ map f arr
    {-# INLINE map #-}                    
    
instance forall m n a. (KnownNatPair m n, Nullary a) => Nullary (MatrixComp m n a) where
    zero = matrixF (\_ -> zero)
    {-# INLINE zero #-}                    

instance forall n a. (KnownNat n, Unital a, Nullary a, Unbox a) => Unital (MatrixComp n n a) where
    one = matrixF kdelta
    {-# INLINE one #-}                    
    
instance forall m n a r. (KnownNatPair m n, ArraySource r a) => Transposable (Matrix r m n a) where
    type Transposed (Matrix r m n a) = MatrixComp n m a
    
    transpose (Matrix arr) = Matrix $ backpermute newExtent swap arr where
            swap (Z :. i :. j) = Z :. j :. i
            newExtent = swap (extent arr)
    {-# NOINLINE transpose #-}
    
instance forall r1 r2 m n p a. (KnownNatTriple m n p, Unbox a, ArraySource r1 a, ArraySource r2 a, Ring a)  => Bimultiplicative (Matrix r1 m n a) (Matrix r2 n p a) where

    bimul m1 m2 = prodidx @m @p |> fmap (\(i,j) -> dot $ cross (i,j) m1 m2) |> matrix
        where
            prodidx::forall m p. KnownNatPair m p =>  [(Int,Int)]
            prodidx = result where
                (m,p) = nat2 @m @p
                result = [(i,j) | i <- [0..m - 1], j <- [0..p - 1]]    
    {-# NOINLINE bimul #-}

instance forall r n a. (KnownNat n, Ring a, ArraySource r a) => Bimultiplicative (Covector r n a) (Vector r n a) where
    bimul (Covector m1) (Vector m2) = v1 >*< v2 where
        v1 = row 1 m1
        v2 = col 1 m2

        
instance forall r1 r2 n m a. (KnownNatPair m n, ArraySource r1 a, ArraySource r2 a, Additive a) => Biadditive (Matrix r1 n m a) (Matrix r2 n m a) where
    biadd (Matrix m1) (Matrix m2) = Matrix $ bimap (+) m1 m2
    {-# INLINE biadd #-}

instance forall n m a. (KnownNatPair m n, ArraySource D a, Additive a) => Additive (MatrixComp n m a) where
    add (Matrix m1) (Matrix m2) = Matrix $ bimap (+) m1 m2
    {-# INLINE add #-}    

instance forall m n a.(KnownNatPair m n, Unbox a) => IsList (MatrixEval m n a) where
    type Item (Matrix U m n a) = a
    
    toList (Matrix m) = Repa.toList m
    {-# INLINE toList #-}

    fromList =   eval . matrix
    {-# INLINE fromList #-}

instance forall m n a.(Unbox a, KnownNatPair m n) => IsList (MatrixComp m n a) where
    type Item (Matrix D m n a) = a
    
    toList (Matrix m) = Repa.toList m
    {-# INLINE toList #-}
    
    fromList src  =  matrix src
    {-# INLINE fromList #-}
    
instance (ArraySource r a, Ring a) => Accumulator (Matrix r m n a) where
    type Accumulation (Matrix r m n a) = a
    accumulate (Matrix arr) = accumulate arr
    {-# INLINE accumulate #-}

type instance Negated (Matrix r m n a) = MatrixComp m n (Negated a)

instance (ArraySource r a, Binegatable a) => Binegatable (Matrix r m n a) where 
    binegate (Matrix arr) = Matrix $ binegate arr 
    {-# INLINE binegate #-}

instance (ArraySource r a) => Indexed (Matrix r m n a) (Int,Int) where    
    at (Matrix arr) (i,j) = arr Repa.! dimension (i,j)
    {-# INLINE at #-}
    
instance (ArraySource r a, Ring a) => Bimultiplicative (DataSlice r a) (DataSlice r a) where    
    bimul (DataSlice v1) (DataSlice v2)  = accumulate $ zip (*) v1 v2
    
instance forall r m n k a. (ArraySource r a, LeftScalar k a) => LeftScalar k (Matrix r m n a) where
    type LeftScaled k (Matrix r m n a) = MatrixComp m n (LeftScaled k a)
    scaleL k (Matrix arr) = Matrix $ scaleL k arr

instance forall r m n k a. (ArraySource r a, RightScalar a k) => RightScalar (Matrix r m n a) k where
    type RightScaled (Matrix r m n a) k = MatrixComp m n (RightScaled a k)
    scaleR (Matrix arr) k = Matrix $ scaleR arr k        

deriving instance (Show a, Unbox a) => Show (TableEval a)
deriving instance (Show a, Unbox a) => Show (SliceEval a)
    
instance forall m n a. (Show a, Unbox a) => Show (MatrixEval m n a) where
    show(Matrix arr) = show arr
    
instance (ArraySource r a) => Computable (Matrix r m n a) where
    type Computation (Matrix r m n a) = MatrixComp m n a
    compute (Matrix arr) = Matrix $ Repa.delay arr
    {-# INLINE compute #-}
    
instance (ArraySource r a) => Indexed (DataTable r a) (Int,Int) where
    at (DataTable arr) (i,j) = arr Repa.! (Z :. i :. j)
    {-# INLINE at #-}
        
instance (Load r DIM1 a, Unbox a) => Evaluatable (DataSlice r a) where
    type Evaluated (DataSlice r a) = SliceEval a
    eval (DataSlice arr) = DataSlice $ Repa.computeS arr        
    {-# INLINE eval #-}            

instance (Load r DIM2 a, Unbox a) => Evaluatable (Matrix r m n a) where
    type Evaluated (Matrix r m n a) = MatrixEval m n a
    eval (Matrix arr) = Matrix $ Repa.computeS arr        
    {-# INLINE eval #-}            
    