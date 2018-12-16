module Alpha.Numerics.Algebra.IndexedTerm
(
    IndexedTerm(..), IndexRange(..),
    
    term,
)
where
import Alpha.Numerics.Base

-- | Represents a term t indexed by i
newtype IndexedTerm i t = IndexedTerm (Func i t)
    deriving (Generic)
instance Newtype (IndexedTerm i t)

-- | Defines the lower and upper bounds for a sequence of 'IndexedTerm' values
newtype IndexRange i = IndexRange (i,i)
    deriving (Eq,Ord,Show,Formattable)

term::(Integral i) => Func i t -> IndexedTerm i t
term = IndexedTerm

instance Formattable (IndexedTerm i t) where
    format _ = "term(i)"

instance Show (IndexedTerm i t) where
    show = string . format
    
    
instance (Ord a, Enum a) => Discrete (IndexRange a) where
    type Individual (IndexRange a) = a

    discretize (IndexRange (i,j)) = [i..j]

type family MultiIndex (i::Nat) a = r | r -> i a where
    MultiIndex 1 a = IndexRange a
    MultiIndex 2 a = UniTuple2 (IndexRange a)
    MultiIndex 3 a = UniTuple3 (IndexRange a)
    MultiIndex 4 a = UniTuple4 (IndexRange a)
    MultiIndex 5 a = UniTuple5 (IndexRange a)

class MultiIndexer (i::Nat) where
    multix::UniTupled a -> MultiIndex i a    

