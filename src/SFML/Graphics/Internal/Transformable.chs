{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.Transformable where


import Control.Applicative ((<$>))
import Foreign (FinalizerPtr, alloca, newForeignPtr, peek, with)
import Foreign.C.Types (CFloat)

{# import SFML.Graphics.Internal.Transform #} (Transform, TransformPtr)
{# import SFML.Graphics.Internal.Types #} (CSFML_Transformable, Transformable(..), TransformablePtr, withTransformable)

{# import SFML.System.Internal.Vector2 #} (Vector2D, Vector2FPtr, fromVector2F, toVector2F)

import SFML.Utility.Foreign (cFloatConv)

#include <SFML/Graphics/Transformable.h>
#include <SFML/Graphics/TransformableWrapper.h>


-- |Create a new `Transformable'.
createTransformable :: IO Transformable
createTransformable =
    {# call unsafe sfTransformable_create #} >>= \res ->
    mkTransformable res

mkTransformable :: TransformablePtr -> IO Transformable
mkTransformable ptr = Transformable <$> newForeignPtr c_destroyTransformable ptr

-- |Create a new `Transformable' from an existing one.
copyTransformable :: Transformable -> IO Transformable
copyTransformable transformable =
    withTransformable transformable $ \t ->
    {# call unsafe sfTransformable_copy #} t >>= \res ->
    mkTransformable res

-- |Destroy a `Transformable'.
foreign import ccall unsafe "SFML/Graphics/Transformable.h &sfTransformable_destroy"
    c_destroyTransformable :: FinalizerPtr CSFML_Transformable

-- |Set the position of a `Transformable'.
setPosition :: Transformable -> Vector2D Float -> IO ()
setPosition transformable position =
    withTransformable transformable $ \t ->
    with (toVector2F position) $ \p ->
    {# call unsafe sfTransformable_setPosition_wrapper #} t p

-- |Set the orientation of a `Transformable'.
setRotation :: Transformable -> Float -> IO ()
setRotation transformable angle =
    withTransformable transformable $ \t ->
    {# call unsafe sfTransformable_setRotation #} t (cFloatConv angle)

-- |Set the scale factors of a `Transformable'.
setScale :: Transformable -> Vector2D Float -> IO ()
setScale transformable scales =
    withTransformable transformable $ \t ->
    with (toVector2F scales) $ \s ->
    {# call unsafe sfTransformable_setScale_wrapper #} t s

-- |Set the local origin of a `Transformable'.
setOrigin :: Transformable -> Vector2D Float -> IO ()
setOrigin transformable origin =
    withTransformable transformable $ \t ->
    with (toVector2F origin) $ \o ->
    {# call unsafe sfTransformable_setOrigin_wrapper #} t o

-- |Get the position of a `Transformable'.
getPosition :: Transformable -> IO (Vector2D Float)
getPosition transformable =
    withTransformable transformable $ \t ->
    alloca $ \position ->
    {# call unsafe sfTransformable_getPosition_wrapper #} t position >>
    fromVector2F <$> peek position

-- |Get the orientation of a `Transformable'.
getRotation :: Transformable -> IO Float
getRotation transformable =
    withTransformable transformable $ \t ->
    {# call unsafe sfTransformable_getRotation #} t >>= \res ->
    return $ cFloatConv res

-- |Get the current scale of a `Transformable'.
getScale :: Transformable -> IO (Vector2D Float)
getScale transformable =
    withTransformable transformable $ \t ->
    alloca $ \scales ->
    {# call unsafe sfTransformable_getScale_wrapper #} t scales >>
    fromVector2F <$> peek scales

-- |Get the local origin of a `Transformable'.
getOrigin :: Transformable -> IO (Vector2D Float)
getOrigin transformable =
    withTransformable transformable $ \t ->
    alloca $ \origin ->
    {# call unsafe sfTransformable_getOrigin_wrapper #} t origin >>
    fromVector2F <$> peek origin

-- |Move a `Transformable' by a given offset.
move :: Transformable -> Vector2D Float -> IO ()
move transformable offset =
    withTransformable transformable $ \t ->
    with (toVector2F offset) $ \o ->
    {# call unsafe sfTransformable_move_wrapper #} t o

-- |Rotate a `Transformable'.
rotate :: Transformable -> Float -> IO ()
rotate transformable angle =
    withTransformable transformable $ \t ->
    {# call unsafe sfTransformable_rotate #} t (cFloatConv angle)

-- |Scale a `Transformable'.
scale :: Transformable -> Vector2D Float -> IO ()
scale transformable factors =
    withTransformable transformable $ \t ->
    with (toVector2F factors) $ \f ->
    {# call unsafe sfTransformable_scale_wrapper #} t f

-- |Get the combined `Transform'. of a `Transformable'.
getTransform :: Transformable -> IO Transform
getTransform transformable =
    withTransformable transformable $ \t ->
    alloca $ \transform ->
    {# call unsafe sfTransformable_getTransform_wrapper #} t transform >>
    peek transform

-- |Get the inverse of the combined `Transform' of a `Transformable'.
getInverseTransform :: Transformable -> IO Transform
getInverseTransform transformable =
    withTransformable transformable $ \t ->
    alloca $ \transform ->
    {# call unsafe sfTransformable_getInverseTransform_wrapper #} t transform >>
    peek transform

