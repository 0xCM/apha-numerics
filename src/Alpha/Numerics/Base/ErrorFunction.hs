-----------------------------------------------------------------------------
-- | Implementation of the error function, derived from  the eponymous
-- package by Lennart Augustsson
-- Copyright   :  (c) Lennart Augustsson
-- License     :  BSD3, per original source
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}

module Alpha.Numerics.Base.ErrorFunction
(
    erf, erfc, erfcx, normcdf, 
    invnormcdf, inverf, inverfc 
) where
import Foreign.C
import Alpha(double,fromDouble, pattern PI)
import Prelude--(realToFrac)

foreign import ccall "erf" c_erf :: CDouble -> CDouble
foreign import ccall "erfc" c_erfc :: CDouble -> CDouble
foreign import ccall "erff" c_erff :: CFloat -> CFloat
foreign import ccall "erfcf" c_erfcf :: CFloat -> CFloat

-- The derivative of 'erf' is @\ x -> 2 / sqrt pi * exp (x^2)@,
-- and this uniquely determines 'erf' by @erf 0 = 0@.
erf::Double -> Double
erf = double . c_erf . fromDouble

erfc::Double -> Double
erfc = double . c_erfc . fromDouble
    
erfcx::Double -> Double
erfcx x = exp (x*x) * erfc x

normcdf::Double -> Double
normcdf x = erfc(-x / sqrt 2) / 2

-- |Inverse error functions, e.g., @inverf . erf = id@ and @erf . inverf = id@ assuming
-- the appropriate codomain for 'inverf'.
-- Note that the accuracy may drop radically for extreme arguments.

invnormcdf::Double -> Double
invnormcdf 0 = -1/0
invnormcdf 1 = 1/0
invnormcdf p =
    -- Do one iteration with Halley's root finder to get a more accurate result.
    let x = inorm p
        e = 0.5 * erfc (-x / sqrt 2) - p
        u = e * sqrt (2*PI) * exp (x*x / 2)
    in  x - u / (1 + x * u / 2)

inverf::Double -> Double
inverf p = inverfc (1 - p)

inverfc::Double -> Double
inverfc p = - invnormcdf (p/2) / sqrt 2

-- Taken from http://home.online.no/~pjacklam/notes/invnorm/
-- Accurate to about 1e-9.
inorm :: Double -> Double
inorm p =
    let a1 = -3.969683028665376e+01
        a2 =  2.209460984245205e+02
        a3 = -2.759285104469687e+02
        a4 =  1.383577518672690e+02
        a5 = -3.066479806614716e+01
        a6 =  2.506628277459239e+00

        b1 = -5.447609879822406e+01
        b2 =  1.615858368580409e+02
        b3 = -1.556989798598866e+02
        b4 =  6.680131188771972e+01
        b5 = -1.328068155288572e+01

        c1 = -7.784894002430293e-03
        c2 = -3.223964580411365e-01
        c3 = -2.400758277161838e+00
        c4 = -2.549732539343734e+00
        c5 =  4.374664141464968e+00
        c6 =  2.938163982698783e+00

        d1 =  7.784695709041462e-03
        d2 =  3.224671290700398e-01
        d3 =  2.445134137142996e+00
        d4 =  3.754408661907416e+00

        pLow = 0.02425

        nan = 0/0

    in  if p < 0 then
            nan
        else if p == 0 then
            -1/0
        else if p < pLow then
            let q = sqrt(-2*log(p))
            in  (((((c1*q+c2)*q+c3)*q+c4)*q+c5)*q+c6) / ((((d1*q+d2)*q+d3)*q+d4)*q+1)
        else if p < 1 - pLow then
            let q = p - 0.5
                r = q*q
            in  (((((a1*r+a2)*r+a3)*r+a4)*r+a5)*r+a6)*q / (((((b1*r+b2)*r+b3)*r+b4)*r+b5)*r+1)
        else if p <= 1 then
            - inorm (1 - p)
        else
            nan
