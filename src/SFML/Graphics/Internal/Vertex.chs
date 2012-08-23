{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.Vertex where


import Control.Applicative ((<$>), (<*>))
import Foreign (Ptr, Storable(..), alloca, with)

{# import SFML.Graphics.Internal.Color #} (Color, ColorPtr)

{# import SFML.System.Internal.Vector2 #} (Vector2D, Vector2FPtr, fromVector2F, toVector2F)


#include <SFML/Graphics/Vertex.h>
#include <SFML/Graphics/VertexWrapper.h>


-- |Define a point with `Color' and `Texture' coordinates.
data Vertex = Vertex 
    { getPosition      :: Vector2D Float  -- ^Position of the `Vertex'.
    , getColor         :: Color           -- ^`Color' of the `Vertex'.
    , getTextureCoords :: Vector2D Float  -- ^Coordinates of the `Texture''s pixel to map to the `Vertex'.
    }
{# pointer *sfVertex as VertexPtr -> Vertex #}

instance Storable Vertex where
    sizeOf    _ = {# sizeof sfVertex  #}
    alignment _ = {# alignof sfVertex #}
    peek p = Vertex
        <$> c_getPosition     p
        <*> c_getColor        p
        <*> c_getTextureCoods p
    poke p (Vertex position color textureCoords) = do
        c_setPosition      p position
        c_setColor         p color
        c_setTextureCoords p textureCoords

c_getPosition :: VertexPtr -> IO (Vector2D Float)
c_getPosition vertex =
    alloca $ \position ->
    {# call unsafe sfVertex_getPosition_wrapper #} vertex position >>
    fromVector2F <$> peek position

c_setPosition :: VertexPtr -> Vector2D Float -> IO ()
c_setPosition vertex position =
    with (toVector2F position) $ \p ->
    {# call unsafe sfVertex_setPosition_wrapper #} vertex p

c_getColor :: VertexPtr -> IO Color
c_getColor vertex =
    alloca $ \color ->
    {# call unsafe sfVertex_getColor_wrapper #} vertex color >>
    peek color

c_setColor :: VertexPtr -> Color -> IO ()
c_setColor vertex color =
    with color $ \c ->
    {# call unsafe sfVertex_setColor_wrapper #} vertex c

c_getTextureCoods :: VertexPtr -> IO (Vector2D Float)
c_getTextureCoods vertex =
    alloca $ \coords ->
    {# call unsafe sfVertex_getTextureCoords_wrapper #} vertex coords >>
    fromVector2F <$> peek coords

c_setTextureCoords :: VertexPtr -> Vector2D Float -> IO ()
c_setTextureCoords vertex coords =
    with (toVector2F coords) $ \c ->
    {# call unsafe sfVertex_setTextureCoords_wrapper #} vertex c

