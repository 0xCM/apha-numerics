{-# LANGUAGE ExtendedDefaultRules #-}
module Alpha.Numerics.Algebra.Permutation
(
    Permutation(..),
    perm,
    switch

)
where
import Alpha.Numerics.Base
import qualified Data.Vector as V
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Alpha.Text.Combinators as TC
import qualified Alpha.Text.Symbols as TC
import Alpha.Text.Symbols(pipe,space)
import Alpha.Canonical.Text(enclose)
import qualified Data.Text as Text
import Prelude(snd)
import qualified Alpha as A
default (Int, Double, Text)

pattern EOL = "\n"::Text

type PermutationMap = Map Int Int

-- Represents a bijective function over the set {1,2...,n}
newtype Permutation n = Permutation PermutationMap
    deriving(Eq,Generic,Functor,Ord)
instance Newtype(Permutation n)

type instance Dom (Permutation n) = Int
type instance Cod (Permutation n) = Int
type instance Element (Permutation n) = Int

-- instance forall n. (KnownNat n) => Morphic (Permutation n) where
--      morphism (Permutation pm) = PairMorphism pm     
--      fx pm i = pm ! i

-- instance forall n. KnownNat n => IsList (Permutation n) where
--     type Item (Permutation n) = Int
--     toList (Permutation perm) = perm |> Map.toAscList |> fmap(\(k,v) -> v)
    
--     fromList::[Int] -> Permutation n
--     fromList list = (list |> A.mapi id) |> perm
    
-- instance forall n. KnownNat n =>  Container (Permutation n)    
    
    
-- | Constructs a permutation of lenth n from an explicit list of correspondences
perm::forall n. KnownNat n => [(Int,Int)] -> Permutation n
perm = Permutation . Map.fromList

image::forall n. KnownNat n => Permutation n -> Int -> Int
image = (!)

preimage::forall n. KnownNat n =>Permutation n -> Int -> Int
preimage (Permutation pm) i =  list pm |> filter (\(k,v) -> k == i) |> fmap(\(k,v) -> k) |> head

-- | Effects a transposition
switch::forall n. KnownNat n => (Int,Int) -> Permutation n -> Permutation n
switch (i,j) p =  unwrap p |> toList |> fmap (\(r,s) -> rule (r,s) ) |> perm 
    where
        rule::(Int,Int) -> (Int,Int)
        rule (r,s) =  if r == i then (r, p ! j)
                      else if r == j then (r, p ! i)   
                      else (r, s)

instance Formattable (Permutation n) where
    format (Permutation perm) = rows where
        row1 = perm |> list |> fmap (\(x,y) -> format x) |> weave space |> enclose pipe pipe
        row2 = perm |> list |> fmap (\(x,y) -> format y) |> weave space |> enclose pipe pipe 
        sep = clone 20 "-"
        rows = sep +++ EOL +++ row1 +++ EOL +++ row2

instance Show (Permutation n) where
    show = string . format

instance forall n a.KnownNat n =>  Indexed (Permutation n) Int where
    at (Permutation s ) i = s Map.! i
    
instance  KnownNat n => Semigroup (Permutation n) where
    g <> (Permutation f) = f |> Map.toList 
                                |> fmap (\(i,j) -> (i, g ! j)) 
                                |> Map.fromList 
                                |> Permutation
                                                   
instance forall n. KnownNat n => Multiplicative (Permutation n) where
    mul g f = g <> f

instance forall n. KnownNat n =>  Unital (Permutation n) where
    one = permutation @n [minBound..maxBound] where
        n = int $ nat @n

        -- Defines a permutation domain consisting of the integers 1..n
        domain::forall n. KnownNat n => NatKSpan 1 n
        domain = natKspan @1 @n

        permutation::forall n. KnownNat n => [Int] -> Permutation n
        permutation range = Permutation  z 
            where        
                s = natKspan @1 @n
                d = domain @n
                pt = points s |> fmap int
                z =  Map.fromList (List.zip pt range)

                
instance forall n. KnownNat n => Invertible (Permutation n) where
    invert (Permutation p) = Permutation $ flip p    

instance forall n. KnownNat n => Monoid (Permutation n) where
    mempty = one

