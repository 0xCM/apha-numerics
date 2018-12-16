-----------------------------------------------------------------------------
-- | Factorial and related operations
-- Copyright   :  (c) Chris Moore
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Numerics.Base.Factorial
(
    factorial
) where
import Alpha
import qualified Data.List as List

factorial::Int -> Integer
factorial val = List.product l where    
    l = [1..integer(val)] :: [Integer]

