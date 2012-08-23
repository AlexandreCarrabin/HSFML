{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module SFML.System.Vector 
    ( Vector2D(..)
    , Vector3D(..)

    , (.++.)
    , (.--.)
    , (.**.)
    , (.//.)

    , (.+++.)
    , (.---.)
    , (.***.)
    , (.///.)
    ) where


import SFML.System.Internal.Vector2
import SFML.System.Internal.Vector3


-- |Add two 2-dimensional vectors.
(.++.) :: Num a => Vector2D a -> Vector2D a -> Vector2D a
Vector2D xa ya .++. Vector2D xb yb = Vector2D (xa + xb) (ya + yb)

-- |Subtract two 2-dimensional vectors.
(.--.) :: Num a => Vector2D a -> Vector2D a -> Vector2D a
Vector2D xa ya .--. Vector2D xb yb = Vector2D (xa - xb) (ya - yb)

-- |Multiply the components of a 2-dimensional vector by a factor.
(.**.) :: Num a => Vector2D a -> a -> Vector2D a
Vector2D x y .**. f = Vector2D (x * f) (y * f)

-- |Divide the components of a 2-dimensional vector by a factor.
(.//.) :: Fractional a => Vector2D a -> a -> Vector2D a
Vector2D x y .//. f = Vector2D (x / f) (y / f)


-- |Add two 3-dimensional vectors.
(.+++.) :: Num a => Vector3D a -> Vector3D a -> Vector3D a
Vector3D xa ya za .+++. Vector3D xb yb zb = Vector3D (xa + xb) (ya + yb) (za + zb)

-- |Subtract two 3-dimensional vectors.
(.---.) :: Num a => Vector3D a -> Vector3D a -> Vector3D a
Vector3D xa ya za .---. Vector3D xb yb zb = Vector3D (xa - xb) (ya - yb) (za - zb)

-- |Multiply the components of a 3-dimensional vector by a factor.
(.***.) :: Num a => Vector3D a -> a -> Vector3D a
Vector3D x y z .***. f = Vector3D (x * f) (y * f) (z * f)

-- |Divide the components of a 3-dimensional vector by a factor.
(.///.) :: Fractional a => Vector3D a -> a -> Vector3D a
Vector3D x y z .///. f = Vector3D (x / f) (y / f) (z / f)

