{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.System.Internal.Vector3 where


import Control.Applicative ((<$>), (<*>))
import Foreign (Ptr, Storable(..))
import Foreign.C.Types (CFloat)

import SFML.Utility.Foreign (cFloatConv)

#include <SFML/System/Vector3.h>


data Vector3D a = Vector3D
    { v3x :: a
    , v3y :: a
    , v3z :: a
    } deriving (Eq, Show)


fromVector3F :: RealFrac a => Vector3F -> Vector3D a
fromVector3F (Vector3F x y z) = Vector3D (cFloatConv x) (cFloatConv y) (cFloatConv z)

toVector3F :: RealFrac a => Vector3D a -> Vector3F
toVector3F (Vector3D x y z) = Vector3F (cFloatConv x) (cFloatConv y) (cFloatConv z)


data Vector3F = Vector3F CFloat CFloat CFloat
{# pointer *sfVector3f as Vector3FPtr -> Vector3F #}

instance Storable Vector3F where
    sizeOf    _ = {# sizeof sfVector3f  #}
    alignment _ = {# alignof sfVector3f #}
    peek p = Vector3F
        <$> {# get sfVector3f->x #} p
        <*> {# get sfVector3f->y #} p
        <*> {# get sfVector3f->z #} p
    poke p (Vector3F x y z) = do
        {# set sfVector3f.x #} p x
        {# set sfVector3f.y #} p y
        {# set sfVector3f.z #} p z

