module Alpha.Numerics.Structure
(
    Structured(..),
    Unital(..),
    Invertible(..),
    Semigroup(..),
    Monoid(..),
    Group(..)
    
)
where
import Alpha(Element(..), Eq(..),Set(..))

class Structured s where
    elements::[Element s]
    
class (Structured s) => Unital s where
    one::Element s

class (Structured s) => Nullary s where
    zero::Element s
    
class (Structured s) => Invertible s where
    invert::Element s -> Element s    

class (Structured s) => Semigroup s where
    (<>)::Element s -> Element s -> Element s

class (Unital s, Semigroup s) => Monoid s where    
    mempty::Element s

class (Monoid s, Invertible s) => Group s