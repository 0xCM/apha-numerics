module Alpha.Numerics.Linear.Computable
(
    Computable(..),
    CompVector, CompTable,
    row,col
)
where
import qualified Data.Array.Repa as Repa
import Data.Array.Repa(Array,Shape,FullShape(..),SliceShape(..),Array,U,D,DIM1,DIM2,slice)
import Data.Array.Repa.Index
import qualified Data.Array.Repa.Algorithms.Matrix as Repa
import qualified Data.Array.Repa.Unsafe as Repa
import qualified Data.Vector as V
import Alpha.Numerics.Base

-- A vector of potential data
type CompVector a = Array D DIM1 a

-- A rectangular array of potential data
type CompTable a = Array D DIM2 a


-- Represents a computation over a linear structure    
data Computable m n a 
    =   VectorComp (CompVector a)     
      | MatrixComp (CompTable a)     
    deriving(Eq)
    
-- | Defines a computation that, when actualized, produces a selected row    
row::forall m n a. (KnownNat m, KnownNat n) => Computable m n a -> Int -> Computable 1 n a
row (MatrixComp arr) ix = VectorComp $ slice arr (Repa.Any :. ix - 1 :. Repa.All)

-- | Defines a computation that, when actualized, produces a selected column        
col::forall m n a. (KnownNat m, KnownNat n) => Computable m n a -> Int -> Computable m 1 a
col (MatrixComp arr) ix = VectorComp $ slice arr (Repa.Any :. ix - 1) where

        
