{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
module Alpha.Numerics.Base
(
    module X,
    IMappable(..),
    ArraySource(..),    
    Array(..),
    Unbox(..),
    Accumulator(..),
    Evaluatable(..),
    prints, eol, factorial,
    pattern EOL,
    
) where
import Alpha as X hiding(
    Matrix, Covector, Vector,Any,All, Tabular(..),dot,Bifunctor(..), Zippable(..), Spanned(..), DataTable(..),
    mapi,row,col,matrix, vector, covector, enclose)

import Alpha.Numerics.Base.ErrorFunction as X
import Alpha.Numerics.Base.Tolerance as X
import Data.Vector.Unboxed(Unbox(..))
import Data.Array.Repa(Array(..))
import qualified Alpha as A
import qualified Data.Array.Repa as Repa
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T

import Prelude(putStrLn)

prints::String -> IO()
prints = putStrLn

pattern EOL = "\n"

eol::Text
eol = "\n"

-- | Classifies a source of array data    
type ArraySource r a = Repa.Source r a


-- | Defines a class of types that represent algorithm applications/evaluations   
class Evaluatable a where
    type Evaluated a

    eval::a -> Evaluated a


class IMappable c a b where
    type MapIndex c a b
    type IMapped c a b
    mapi::((MapIndex c a b,a) -> b) -> c -> IMapped c a b
    
instance IMappable [a] a b where
    type MapIndex [a] a b = Int
    type IMapped [a] a b = [b]
    
    mapi::((Int,a) -> b) -> [a] -> [b]
    mapi = A.mapi

factorial::Int -> Integer
factorial val = List.product l where    
    l = [1..integer(val)] :: [Integer]
    
