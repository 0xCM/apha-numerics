{-# LANGUAGE ExtendedDefaultRules #-}

module Alpha.Numerics.Examples.Linear where
import Alpha.Numerics
import Alpha.Structures.Linear
import qualified Data.List as L
default(Int,Double,Text)    

instance Example "l-1" where
    example = do
        let m =  matrix @3 @3 [1..9] |> eval
        let m1 = transpose m |> eval
        let m2 = transpose m1 |> eval
        print m
        print m1
        print m2
        print (m1 == m2)
        --print m1 == m2

instance Example "l-2" where
    example = do
        let m1 = matrix @2 @3 [2,4,6,8,10,12]
        let m2 = matrix @3 @2 [1,3,5,7,9,11]
        let result1 =  m1 >*< m2 |> eval
        print result1
    
instance Example "l-3" where
    example = do
        let mat33 = matrix @3 @3 [1..9] 
        let test =  mat33 !! (2,2) 
        print test


instance Example "l-4" where
    example = do    
        let m = matrix @5 @5 [1..25] |> eval
        let sm = submatrix @0 @0 @2 @2 m |> eval
        print sm
   
instance Example "l-5" where
    example = do
        let m1 = matrix @3 @3 [0..8] 
        print (eval m1)

instance Example "l-6" where
    example = do
        let m0 = zero::Matrix D 3 3 Int
        let m1 = one::Matrix D 3 3 Int
        print (eval m0)
        print (eval m1)


instance Example "l-7" where
    example = do
        let m1 = matrix @2 @2 [1, 2, 3, 4]
        let m2 = matrix @2 @2 [-1, -2, -3, -4]
        let m = m1 >+< m2
        print (eval m)
    
example8::IO()
example8 = do
    let m = (matrix' @[Int] @2 @2 [1,2,3,4]) + (matrix @2 @2 [-1, -2, -3, -4])    
    print (eval m)

-- example9::IO()
-- example9 = do
--     let m = matrix @2 @2 [1, 0, 0, 1]
--     let s1 = m .* 4
--     let s2 = 8 *. m
--     print (eval s1 ) 
--     print (eval s2 )
    
example10::IO()
example10 = do 
    let m = matrix @3 @3 (L.replicate 9 1)
    let f ((i,j), x) = ifelse (i == j) x 0
    let mapped = mapi f m
    print (eval mapped)

example11::IO()
example11 = do
    let m = matrix @3 @3 [1..9] 
    let c = cols m
    let r = rows m
    prints "Input matrix:"
    print (eval m)
    prints "Rows:"
    print (L.map eval r) 
    prints "Columns"
    print (L.map eval c) 

example12::IO()
example12 = do
    let m = matrixF @3 @3 (kdelta @Int)
    print (eval m)
    
        