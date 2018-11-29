module Alpha.Numerics.Base
(
    module X,
    Unbox(..),
    nat,
    NatPair(..), nat2,
    NatTriple(..), nat3,
    NatQuad(..), nat4
) where
import Alpha as X hiding(Matrix,row,col,matrix, vector, covector, Covector, Vector,Any,All, Zippable(..))
import Alpha.Numerics.Base.ErrorFunction as X
import Alpha.Numerics.Base.Tolerance as X
import Data.Vector.Unboxed

-- | Alias for a pair of 'KnownNat' constraints
type NatPair m n = (KnownNat m, KnownNat n)  

-- | Alias for a truple of 'KnownNat' constraints
type NatTriple m n p = (NatPair m n, KnownNat p)

type NatQuad m n p q = (NatPair m n, NatPair p q)

-- | Computes the 'Int' value corresponding to a type-level nat
nat::forall m. KnownNat m => Int
nat = natVal (proxy @m) |> int

-- | Computes a pair of 'Int' values corresponding to a pair of type-level nats
nat2::forall m n. NatPair m n => (Int,Int)
nat2 = (nat @m, nat @n)

-- | Computes a triple of 'Int' values corresponding to a triple of type-level nats
nat3::forall m n p. NatTriple m n p => (Int,Int,Int)
nat3 = (nat @m, nat @n, nat @p)

nat4::forall m n p q. NatQuad m n p q => (Int,Int,Int,Int)
nat4 = (nat @m, nat @n, nat @p, nat @q)

