{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.Transform where


import Control.Applicative ((<$>))
import Foreign (Ptr, Storable(..), alloca, allocaArray, peek, peekArray, with, withArray)
import Foreign.C.Types (CFloat)

{# import SFML.Graphics.Internal.Rect #} (Rect, fromFloatRect, toFloatRect, FloatRectPtr)

{# import SFML.System.Internal.Vector2 #} (Vector2D(..), Vector2FPtr, fromVector2F, toVector2F)

import SFML.Utility.Foreign (cFloatConv)

#include <SFML/Graphics/Transform.h>
#include <SFML/Graphics/TransformWrapper.h>


-- |A `Transform' represents a space transformation from a 3x3 matrix.
data Transform = Transform {matrix :: [Float]}
{# pointer *sfTransform as TransformPtr -> Transform #}

instance Storable Transform where
    sizeOf    _ = {# sizeof sfTransform #}
    alignment _ = {# alignof sfTransform #}
    peek p = Transform <$> c_getInternalMatrix p
    poke p transform = c_setInternalMatrix p (matrix transform)

c_getInternalMatrix :: TransformPtr -> IO [Float]
c_getInternalMatrix transform =
    {# call unsafe sfTransform_getInternalMatrix #} transform >>= \res ->
    map cFloatConv <$> peekArray 9 res

c_setInternalMatrix :: TransformPtr -> [Float] -> IO ()
c_setInternalMatrix transform array =
    withArray (map cFloatConv array) $ \a ->
    {# call unsafe sfTransform_setInternalMatrix #} transform a


-- |Identity transform (does nothing).
identityTransform :: Transform
identityTransform = Transform [1, 0, 0, 0, 1, 0, 0, 0, 1]


--FIXME: Row or column first ?
--FIXME: Create the transform from a 9-elements matrix ?
-- |Create a new `Transform' from a matrix.
fromMatrix :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Transform
fromMatrix a00 a01 a02 a10 a11 a12 a20 a21 a22 =
    Transform [a00, a01, a02, a10, a11, a12, a20, a21, a22]

-- |Return the 4x4 matrix of a `Transform'. 
--This function fills an array of 16 floats with the transform converted as a 4x4 matrix,
--which is directly compatible with OpenGL functions.
getMatrix :: Transform -> IO [Float]
getMatrix transform =
    with transform $ \t ->
    allocaArray 16 $ \array ->
    {# call unsafe sfTransform_getMatrix #} t array >>
    map cFloatConv <$> peekArray 16 array

-- |Return the inverse of a `Transform'. If the inverse cannot be computed, the identity transform is returned.
getInverse :: Transform -> IO Transform
getInverse transform =
    with transform $ \t ->
    alloca $ \result ->
    {# call unsafe sfTransform_getInverse_wrapper #} t result >>
    peek result

-- |Apply a `Transform' to a 2D point.
transformPoint :: Transform -> Vector2D Float -> IO (Vector2D Float)
transformPoint transform point =
    with transform $ \t ->
    with (toVector2F point) $ \p ->
    alloca $ \result ->
    {# call unsafe sfTransform_transformPoint_wrapper #} t p result >>
    fromVector2F <$> peek result

-- |Apply a `Transform' to a `Rect'.
transformRect :: Transform -> Rect Float -> IO (Rect Float)
transformRect transform rect =
    with transform $ \t ->
    with (toFloatRect rect) $ \r ->
    alloca $ \result ->
    {# call unsafe sfTransform_transformRect_wrapper #} t r result >>
    fromFloatRect <$> peek result

-- |Combine two `Transform's.
--The result is a transform that is equivalent to applying the first transform followed by the second.
--Mathematically, it is equivalent to a matrix multiplication.
combine :: Transform -> Transform -> IO ()
combine transform other =
    with transform $ \t ->
    with other $ \o ->
    {# call unsafe sfTransform_combine #} t o

-- |Combine a `Transform' with a translation.
translate :: Transform -> Vector2D Float -> IO ()
translate transform (Vector2D x y) =
    with transform $ \t ->
    {# call unsafe sfTransform_translate #} t (cFloatConv x) (cFloatConv y)

-- |Combine the current `Transform' with a rotation.
rotate :: Transform -> Float -> IO ()
rotate transform angle =
    with transform $ \t ->
    {# call unsafe sfTransform_rotate #} t (cFloatConv angle)

-- |Combine the current `Transform' with a rotation of given center.
rotateWithCenter :: Transform 
                 -> Float          -- ^Rotation angle, in degrees.
                 -> Vector2D Float  -- ^X and Y coordinates of the center of rotation.
                 -> IO ()
rotateWithCenter transform angle (Vector2D x y) =
    with transform $ \t ->
    {# call unsafe sfTransform_rotateWithCenter #} t (cFloatConv angle) (cFloatConv x) (cFloatConv y)

-- |Combine the current `Transform' with a scaling.
scale :: Transform -> Vector2D Float -> IO ()
scale transform (Vector2D scaleX scaleY) =
    with transform $ \t ->
    {# call unsafe sfTransform_scale #} t (cFloatConv scaleX) (cFloatConv scaleY)

-- |Combine the current `Transform' with a scaling of given center.
scaleWithCenter :: Transform 
                -> Vector2D Float  -- ^Scaling factors on X and Y axis.
                -> Vector2D Float   -- ^X and Y coordinates of the center of scaling.
                -> IO ()
scaleWithCenter transform (Vector2D scaleX scaleY) (Vector2D centerX centerY) =
    with transform $ \t ->
    {# call unsafe sfTransform_scaleWithCenter #} t (cFloatConv scaleX) (cFloatConv scaleY) (cFloatConv centerX) (cFloatConv centerY)

