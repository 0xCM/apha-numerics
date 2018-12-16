module Alpha.Numerics.Linear.Adapters
(
    Array1D(..), Array2D(..), ArrayComp(..), ArrayEval(..),    
    DIM1, DIM2, U, D,
    zip, bimap,subtable,
    Repa.extent, Repa.backpermute, 
)
where
import Alpha.Numerics.Base
import Data.Array.Repa(DIM1,DIM2,Array(..), U,D,Shape(..),extent)    
import Data.Array.Repa.Index(Z(..), (:.)(..), (:.))
import Data.Array.Repa.Eval(Load(..),Target(..))
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Algorithms.Matrix as Repa

import Alpha.Numerics.Linear.Shapes

type instance Element (Array r s a) = a

-- Alias for a 1D array
type Array1D r a = Array r DIM1 a

-- Alias for a 2D array
type Array2D r a = Array r DIM2 a

-- Alias for a computation array
type ArrayComp s a = Array D s a

-- Alias for a (concrete) evaluation array
type ArrayEval s a = Array U s a

-- Produces an array computation from two arrays using a supplied combiner
zip::(Shape s, ArraySource r1 a, ArraySource r2 b) => (a -> b -> c) -> Array r1 s a -> Array r2 s b -> ArrayComp s c
zip f a1 a2 = Repa.zipWith f a1 a2
{-# INLINE zip #-}

bimap::(Shape s, ArraySource r1 a, ArraySource r2 b) => (a -> b -> c) -> Array r1 s a -> Array r2 s b -> ArrayComp s c
bimap f a1 a2 =  g where
    g = Repa.traverse2 a1 a2 (\_ s -> s) eval
    s1 = extent a1
    s2 = extent a2    
    eval f1 f2 s =  f (f1 s) (f2 s)
{-# INLINE bimap #-}

subtable::(ArraySource r a) => Region ->  Array2D r a -> ArrayComp DIM2 a
subtable (Region (rc1 , rc2)) arr =  Repa.extract rc1 rc2 arr 
{-# INLINE subtable #-}

-- instance Structured (Array r s a) where
--     type Element (Array r s a) = a

    
instance (Shape s, ArraySource r a, LeftScalar k a) => LeftScalar k (Array r s a) where 
    type LeftScaled k (Array r s a) = ArrayComp s (LeftScaled k a)
    scaleL k arr = Repa.map (\e -> k *. e  ) arr
    {-# INLINE scaleL #-}
    
instance (Shape s, ArraySource r a, RightScalar a k) => RightScalar (Array r s a) k where 
    type RightScaled (Array r s a) k = ArrayComp s (RightScaled a k)
    scaleR arr k = Repa.map (\e -> e .* k  ) arr
    {-# INLINE scaleR #-}

instance (Shape s, ArraySource r a, Ring a) => Accumulator (Array r s a) where
    type Accumulation (Array r s a) = a
    accumulate = Repa.foldAllS (+) zero 
    {-# INLINE accumulate #-}

instance (Shape s, ArraySource r a) => Mappable (Array r s a) a b where
    type Mapped (Array r s a) a b = (ArrayComp s b)
    map = Repa.map
    {-# INLINE map #-}

instance (Shape s, ArraySource r a) => IMappable (Array r s a) a b where
    type MapIndex (Array r s a) a b = s
    type IMapped (Array r s a) a b = (ArrayComp s b)     
    mapi f arr = tr where
        eval f1 s  = f (s, (f1 s))
        tr = Repa.traverse arr id eval
    {-# INLINE mapi #-}        

instance (ArraySource r a) => Indexed (Array r DIM1 a) Int where
    at arr i = arr Repa.! (Z :. i)
    {-# INLINE at #-}

type instance Negated (Array r s a) = ArrayComp s (Negated a)
    
instance (Shape s, ArraySource r a, Binegatable a) => Binegatable (Array r s a) where 
    binegate arr = Repa.map binegate arr
    {-# INLINE binegate #-}    