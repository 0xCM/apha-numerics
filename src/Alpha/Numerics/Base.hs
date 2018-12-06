{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
module Alpha.Numerics.Base
(
    module X,
    IMappable(..),
    Computable(..),
    Evaluatable(..),
    Mappable(..),
    ArraySource(..),    
    Array(..),
    Unbox(..),
    Accumulator(..),
    Listing(..),
    prints, eol, factorial,
    pattern EOL,
    
) where
import Alpha as X hiding(Matrix, mapi,row,col,matrix, vector, covector, enclose,
    Covector, Vector,Any,All, Tabular(..),dot,Bifunctor(..), Computable(..), Zippable(..), Spanned(..)   
    )
import Alpha.Numerics.Base.ErrorFunction as X
import Alpha.Numerics.Base.Tolerance as X
import Data.Vector.Unboxed(Unbox(..))
import Data.Array.Repa(Array(..))
import qualified Data.Array.Repa as Repa
import qualified Data.List as List
import qualified Data.Map as Map

import Prelude(putStrLn)

prints::String -> IO()
prints = putStrLn

pattern EOL = "\n"

eol::Text
eol = "\n"

-- | Classifies a source of array data    
type ArraySource r a = Repa.Source r a


-- | Defines a class of types that represent algorithm definitions 
class Computable a where
    type Computation a

    -- Defines a computation parameterized
    comp::a -> Computation a

-- | Defines a class of types that represent algorithm applications/evaluations   
class Evaluatable a where
    type Evaluated a

    eval::a -> Evaluated a

class Mappable c a b where    
    type Mapped c a b
    map::(a -> b) -> c -> Mapped c a b

instance Mappable [a] a b where
    type Mapped [a] a b = [b]
    map = List.map

class IMappable c a b where
    type MapIndex c a b
    type IMapped c a b
    mapi::((MapIndex c a b,a) -> b) -> c -> IMapped c a b

instance Listing (Map a b) where
    type ListItem (Map a b) = (a,b)
    list = Map.toList

factorial::Int -> Integer
factorial val = List.product l where    
    l = [1..integer(val)] :: [Integer]
    
