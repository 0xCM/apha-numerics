-----------------------------------------------------------------------------
-- | Defines core linear algebra domain vocabulary
-- Copyright   :  (c) Lennart Augustsson
-- License     :  BSD3, per original source
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Numerics.Linear.Structures
(
    DataTable(..), TableComp(..), TableEval(..),
    DataSlice(..), SliceComp(..), SliceEval(..),

    Matrix(..), MatrixComp(..), MatrixEval(..),
    Vector(..), VectorComp(..), VectorEval(..),
    Covector(..), CovectorComp(..), CovectorEval(..),            

    MatrixSource(..), ElementFunc(..),
)
where
import Alpha.Numerics.Linear.Shapes
import Alpha.Numerics.Linear.Adapters
import Alpha.Numerics.Base
    
import qualified Data.Array.Repa.Algorithms.Matrix as M
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.Index(Z(..), (:.)(..), (:.))
    
-- Defines a table of data without regard to matrix semantics or type-level dimension
newtype DataTable r a = DataTable (Array2D r a)
    deriving (Eq, Generic)
instance Newtype (DataTable r a)        

-- Defines a computation over a table of data
type TableComp a = DataTable D a

-- Defines a concrete table of data
type TableEval a = DataTable U a

-- Defines a linear span of data without regard to vector semantics or type-level dimension
newtype DataSlice r a = DataSlice (Array1D r a)
    deriving (Eq, Generic)
instance Newtype (DataSlice r a)

-- Represents a computation over a slice of data
type SliceComp a = DataSlice D a

-- Represents a concrete span of data
type SliceEval a = DataSlice U a

-- Defines a matrix with type-level dimension
newtype Matrix r m n a = Matrix (Array r DIM2 a)
    deriving (Eq, Generic)
instance Newtype (Matrix r m n a)

-- Represents a matrix algorithm/computation
type MatrixComp m n a = Matrix D m n a

-- Represents a concrete matrix
type MatrixEval m n a = Matrix U m n a

-- Defines a vector with type-level dimension
newtype Vector r m a = Vector (Matrix r m 1 a)
    deriving (Eq, Generic)
instance Newtype (Vector r m a)

-- Represents a vector algorithm/computation
type VectorComp n a = Vector D n a

-- Represents a concrete vector
type VectorEval n a = Vector U n a

-- Defines a covector with type-level dimension
newtype Covector r n a = Covector (Matrix r 1 n a)
    deriving (Eq, Generic)
instance Newtype (Covector r n a)    

-- Represents a covector algorithm/computation
type CovectorComp n a = Covector D n a

-- Represents a concrete covector
type CovectorEval n a = Covector U n a

type ElementFunc a = (DimIx,DimIx) -> a


    
    

class MatrixSource s where  
    type MatrixElement s  
    matrix'::forall m n. NatPair m n => s -> MatrixComp m n (MatrixElement s)
