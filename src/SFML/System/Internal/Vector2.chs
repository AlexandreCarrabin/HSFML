{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.System.Internal.Vector2 where


import Control.Applicative ((<$>), (<*>))
import Foreign (Ptr, Storable(..))
import Foreign.C.Types (CFloat, CInt, CUInt)

import SFML.Utility.Foreign (cFloatConv, cIntConv)

#include <SFML/System/Vector2.h>


data Vector2D a = Vector2D
    { v2x :: a 
    , v2y :: a
    } deriving (Eq, Show)


fromVector2I :: Integral a => Vector2I -> Vector2D a
fromVector2I (Vector2I x y) = Vector2D (cIntConv x) (cIntConv y)

toVector2I :: Integral a => Vector2D a -> Vector2I
toVector2I (Vector2D x y) = Vector2I (cIntConv x) (cIntConv y)

fromVector2U :: Integral a => Vector2U -> Vector2D a
fromVector2U (Vector2U x y) = Vector2D (cIntConv x) (cIntConv y)

toVector2U :: Integral a => Vector2D a -> Vector2U
toVector2U (Vector2D x y) = Vector2U (cIntConv x) (cIntConv y)

fromVector2F :: RealFrac a => Vector2F -> Vector2D a
fromVector2F (Vector2F x y) = Vector2D (cFloatConv x) (cFloatConv y)

toVector2F :: RealFrac a => Vector2D a -> Vector2F
toVector2F (Vector2D x y) = Vector2F (cFloatConv x) (cFloatConv y)


data Vector2I = Vector2I CInt CInt
{# pointer *sfVector2i as Vector2IPtr -> Vector2I #}

instance Storable Vector2I where
    sizeOf    _ = {# sizeof sfVector2i  #}
    alignment _ = {# alignof sfVector2i #}
    peek p = Vector2I
        <$> {# get sfVector2i->x #} p
        <*> {# get sfVector2i->y #} p
    poke p (Vector2I x y) = do
        {# set sfVector2i.x #} p x
        {# set sfVector2i.y #} p y


data Vector2U = Vector2U CUInt CUInt
{# pointer *sfVector2u as Vector2UPtr -> Vector2U #}

instance Storable Vector2U where
    sizeOf    _ = {# sizeof sfVector2u  #}
    alignment _ = {# alignof sfVector2u #}
    peek p = Vector2U
        <$> {# get sfVector2u->x #} p
        <*> {# get sfVector2u->y #} p
    poke p (Vector2U x y) = do
        {# set sfVector2u.x #} p x
        {# set sfVector2u.y #} p y


data Vector2F = Vector2F CFloat CFloat
{# pointer *sfVector2f as Vector2FPtr -> Vector2F #}

instance Storable Vector2F where
    sizeOf    _ = {# sizeof sfVector2f  #}
    alignment _ = {# alignof sfVector2f #}
    peek p = Vector2F
        <$> {# get sfVector2f->x #} p
        <*> {# get sfVector2f->y #} p
    poke p (Vector2F x y) = do
        {# set sfVector2f.x #} p x
        {# set sfVector2f.y #} p y

