module Alpha.Numerics.Algebra.SymmetricGroup
(
    SymmetricGroup, symgroup
)
where
import Alpha.Numerics.Base hiding(Unital, one, Invertible,Semigroup,Monoid,Group)
import qualified Data.List as List
import qualified Data.Map as Map
import Alpha.Numerics.Algebra.Permutation
import Alpha.Numerics.Structure
import qualified Alpha.Canonical.Algebra as A
import qualified Data.Semigroup as S
import qualified Data.Monoid as M
import qualified Alpha.Text.Combinators as TC
import qualified Alpha.Text.Symbols as TC
import qualified Alpha.Text.Asci as TC
import qualified Data.Text as Text

-- Represents the set of all permutations on a set with n elements    
data SymmetricGroup n = SymmetricGroup [Permutation n]

-- Constructs the symmetric group of degree n        
symgroup::forall n.KnownNat n => SymmetricGroup n
symgroup = sg where
    symbols = [1..nat @n]
    perms = List.permutations symbols
    sg = perms |> fmap (\perm ->  (List.zip symbols perm) ) 
               |> fmap (\perm -> Map.fromList perm) 
               |> fmap Permutation
               |> SymmetricGroup

instance forall n. KnownNat n => Eq (SymmetricGroup n) where
    g1 == g2 = s1 == s2 where
        s1 = members g1
        s2 = members g2
            
instance forall n. KnownNat n => Formattable (SymmetricGroup n) where
    format (SymmetricGroup sg) 
        = [TC.Su, n, TC.Colon, EOL, items] |> Text.concat  where        
            n = format (nat @n) 
            items = sg |> fmap (\p -> format p) |> TC.intersperse '\n' 

instance forall n. KnownNat n  => Listing (SymmetricGroup n) where   
    type ListItem (SymmetricGroup n)  = Permutation n
    list (SymmetricGroup sg) = fmap (\x -> unwrap x) sg |> fmap Permutation
        
instance forall n. KnownNat n  => Membership (SymmetricGroup n) where
    type Member (SymmetricGroup n) = Map Int Int
    members (SymmetricGroup sg) = sg |> fmap (\x -> unwrap x) |> set

instance forall n. KnownNat n => Structured (SymmetricGroup n) where
    type Element (SymmetricGroup n) = Permutation n

instance forall n. KnownNat n => Unital (SymmetricGroup n) where
    one::Permutation n
    one = A.one
        
instance forall n. KnownNat n => Invertible (SymmetricGroup n) where
    invert::Permutation n -> Permutation n
    invert = A.invert

instance forall n. KnownNat n => Semigroup (SymmetricGroup n) where
    (<>)::BinaryOperator (Permutation n)
    (<>) = (S.<>)
    
instance forall n. KnownNat n => Monoid (SymmetricGroup n) where
    mempty::Permutation n
    mempty = A.one
    
instance forall n. KnownNat n => Group (SymmetricGroup n)

instance forall n. KnownNat n => Indexed (SymmetricGroup n) Int where
    type Found (SymmetricGroup n) Int = Permutation n
    lookup sg i = (list sg) List.!! i

instance forall n. KnownNat n => Counted (SymmetricGroup n) where
    count _ = factorial (nat @n) |> fromIntegral
