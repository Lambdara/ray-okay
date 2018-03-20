-- Allows making instances for type aliases
{-# LANGUAGE FlexibleInstances #-}

module Algebra where

type Vec = (Float,Float,Float) -- | Used as 3D vector, 3D coordinate, or RGB color
type Float2 = (Float, Float) -- | 2D coordinate
type Ray = (Vec, Vec) -- | Origin and direction

-- | Converts int to a float
intToFloat :: Int -> Float
intToFloat = fromInteger . fromIntegral

-- | apply an argument twice
apply2 :: (a -> a -> b) -> a -> b
apply2 f x = f x x

-- | Applies the operator pairwise on the vector
vCombine :: (Float -> Float -> Float) -> Vec -> Vec -> Vec
vCombine f (u, v, w) (x, y, z) = (f u x, f v y, f w z)

-- | Zero vector
v0 :: Vec
v0 = (0,0,0)

-- | Maps the function over all numbers in the vector
vMap :: (Float -> Float) -> Vec -> Vec
vMap f = vCombine (const f) v0

-- | Sets sign of the vector 
aim :: Vec -> Vec -> Vec
aim target vec = if target <> vec > 0 then vec else (-vec)

-- | Creates vector where x = y = z
rep :: Float -> Vec
rep x = (x,x,x)

instance Num Vec where
 (+) = vCombine (+)
 (-) = vCombine (-)
 (*) = vCombine (*)
 abs = vMap abs
 signum = vMap signum
 fromInteger x = rep $ fromInteger x

-- | From two points to a ray through those points starting at `a`
rayTo :: Vec -> Vec -> Ray
rayTo a b = (a, b - a)

-- | Lazy multiplication
(.*) 0 = const 0
(.*) x = (x*)

-- | Operators for scaling a vector by multiplication or division
(@*), (@/) :: Vec -> Float -> Vec
(@*) v = (.* v) . rep
(@/) v = (v @*) . (1/)

-- | Sum of vector components
vSum :: Vec -> Float
vSum (a,b,c) = a+b+c

-- | Dot product
infix 8 <>
(<>) :: Vec -> Vec -> Float
(<>) a = vSum . (a*)

-- | Shift items of an vector
s (a,b,c) = (b,c,a)
s2 = s . s

-- | Cross product
infix 7 ><
(><) :: Vec -> Vec -> Vec
(><) x y = s x * s2 y - s2 x * s y

-- | Returns the squared length of a Vec
vLengthSquared :: Vec -> Float
vLengthSquared = apply2 (<>)

-- | Returns the length of a Vec
vLength :: Vec -> Float
vLength = sqrt . vLengthSquared

-- | Returns the normalized version of a Vec
normalize :: Vec -> Vec
normalize = apply2 $ flip (@/) . vLength
