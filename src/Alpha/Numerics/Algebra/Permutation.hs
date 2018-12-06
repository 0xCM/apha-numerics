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
import Alpha.Text.Combinators(enclose)
import Alpha.Text.Symbols(pipe)
import qualified Data.Text as Text


type PermutationMap = Map Int Int

-- Represents a bijective function over the set {1,2...,n}
newtype Permutation n = Permutation PermutationMap
    deriving(Eq,Generic,Functor,Ord)
instance Newtype(Permutation n)

type instance Dom (Permutation n) = Int
type instance Cod (Permutation n) = Int
instance forall n. (KnownNat n) => Morphic (Permutation n) where
     morphism (Permutation pm) = PairMorphism pm
     
     fx::Permutation n -> Int -> Int
     fx pm i = pm ! i

-- | Constructs a permutation of lenth n from an explicit list of correspondences
perm::forall n. KnownNat n => [(Int,Int)] -> Permutation n
perm = Permutation . Map.fromList


-- | Effects a transposition
switch::forall n. KnownNat n => (Int,Int) -> Permutation n -> Permutation n
switch (i,j) p =  unwrap p |> toList |> fmap (\(r,s) -> rule (r,s) ) |> perm 
    where
        rule::(Int,Int) -> (Int,Int)
        rule (r,s) =  if r == i 
                        then (r, p ! j)
                    else if r == j 
                        then (r, p ! i)   
                    else 
                        (r, s)
instance Formattable (Permutation n) where
    format (Permutation perm) = rows where
        row1 = perm |> list |> fmap (\(x,y) -> format x) |> (TC.intersperse ' ') |> enclose pipe pipe
        row2 = perm |> list |> fmap (\(x,y) -> format y) |> (TC.intersperse ' ') |> enclose pipe pipe 
        sep = Text.replicate 20 "-"
        rows = [sep, eol, row1, eol,  row2] |> TC.splat

instance Show (Permutation n) where
    show = string . format

instance forall n a.KnownNat n =>  Indexed (Permutation n) Int where
    type Found (Permutation n) Int = Int
    lookup (Permutation s ) i = s Map.! i
    
instance  KnownNat n => Semigroup (Permutation n) where
    g <> (Permutation f) = f |> Map.toList 
                                |> fmap (\(i,j) -> (i, g ! j)) 
                                |> Map.fromList 
                                |> Permutation
                                                        
instance forall n. KnownNat n =>  Unital (Permutation n) where
    one = permutation @n [minBound..maxBound] where
        n = int $ nat @n

        -- Defines a permutation domain consisting of the integers 1..n
        domain::forall n. KnownNat n => NatSpan 1 n
        domain = natspan @1 @n

        permutation::forall n. KnownNat n => [Int] -> Permutation n
        permutation range = Permutation  z 
            where        
                (NatSpan s) = natspan @1 @n
                d = domain @n
                z =  Map.fromList (List.zip s range)

                
instance forall n. KnownNat n => Invertible (Permutation n) where
    invert (Permutation p) = Permutation $ flip p    

instance forall n. KnownNat n => Monoid (Permutation n) where
    mempty = one

testIt::Int -> Int
testIt i | i > 0   = i
         | i < 0   = -1
         | i == 0  = 0

-- instance forall n. KnownNat n => NaturallyPowered (Permutation n) where
--     pow p i | i > 0  = p (pow p (i - 1))
--             | i == 0 = 1
