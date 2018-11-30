{-# LANGUAGE ExtendedDefaultRules #-}

module Alpha.Numerics.Examples.Linear where
import Alpha.Numerics
default(Int,Double)    

example1::IO()
example1 = do
    let m =  matrixU @3 @1 [1,2,3]
    print m
    let mt = transpose m |> compute
    print mt

example2::IO()
example2 = do
    let m1 = matrixU @2 @3 [2,4,6,8,10,12]
    let m2 = matrixU @3 @2 [1,3,5,7,9,11]
    let result =  m1 >*< m2 |> compute
    print result
    
example3::IO()   
example3 = do
    let mat33 = matrixU @3 @3 [1..9] 
    let test =  mat33 ! (2,2) 
    print test

-- example4::IO()
-- example4 = do    
--     let m = matrixD @5 @5 [1..25] |> compute
--     let sm = submatrix @0 @0 @2 @2 m |> compute
--     print sm
    
-- example5::IO()
-- example5 = do    
--     let z = zero @(Matrix D 3 4 Int) |> compute
--     print z
    
