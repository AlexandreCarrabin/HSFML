{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.Circle where


import Control.Applicative ((<$>), (<*>))
import Data.Word (Word)
import Data.IORef (newIORef, writeIORef)
import Foreign (FinalizerPtr, alloca, newForeignPtr, nullPtr, peek, with)
import Foreign.C.Types (CFloat, CInt, CUInt)

{# import SFML.Graphics.Internal.Rect #} (FloatRectPtr, IntRectPtr, Rect, fromFloatRect, fromIntRect, toIntRect)
{# import SFML.Graphics.Internal.Texture #} (mkConstTexture)
{# import SFML.Graphics.Internal.Transform #} (Transform, TransformPtr)
{# import SFML.Graphics.Internal.Color #} (Color, ColorPtr)
{# import SFML.Graphics.Internal.Types #} (Circle(..), CirclePtr, CSFML_Circle, withCircle , Texture, TexturePtr, withTexture)

{# import SFML.System.Internal.Vector2 #} (Vector2D, Vector2FPtr, fromVector2F, toVector2F)

import SFML.Utility.Foreign (cFloatConv, cIntConv, cIntFromBool, ptrToMaybe)

#include <SFML/Graphics/CircleShape.h>
#include <SFML/Graphics/CircleShapeWrapper.h>


-- |Create a new `Circle'.
createCircle :: IO Circle
createCircle =
    {# call unsafe sfCircleShape_create #} >>= \res ->
    mkCircle res

mkCircle :: CirclePtr -> IO Circle
mkCircle ptr = Circle <$> newForeignPtr c_destroyCircle ptr <*> newIORef Nothing

-- |Create a new `Circle' by copying an existing one.
copyCircle :: Circle -> IO Circle
copyCircle circle =
    withCircle circle $ \c ->
    {# call unsafe sfCircleShape_copy #} c >>= \copy ->
    getTexture circle >>= \texture ->
    Circle <$> newForeignPtr c_destroyCircle copy <*> newIORef texture

-- |Destroy a `Circle'.
foreign import ccall unsafe "SFML/Graphics/CircleShape.h &sfCircleShape_destroy"
    c_destroyCircle :: FinalizerPtr CSFML_Circle

-- |Set the position of the `Circle'. This function completely overwrites the previous position.
setPosition :: Circle -> Vector2D Float -> IO ()
setPosition circle position =
    withCircle circle $ \c ->
    with (toVector2F position) $ \p ->
    {# call unsafe sfCircleShape_setPosition_wrapper #} c p

-- |Set the orientation of the `Circle'. This function completely overwrites the previous rotation.
setRotation :: Circle -> Float -> IO ()
setRotation circle angle =
    withCircle circle $ \c ->
    {# call unsafe sfCircleShape_setRotation #} c (cFloatConv angle)

-- |Set the scale factors of the `Circle'. This function completely overwrites the previous scale.
setScale :: Circle -> Vector2D Float -> IO ()
setScale circle scales =
    withCircle circle $ \c ->
    with (toVector2F scales) $ \s ->
    {# call unsafe sfCircleShape_setScale_wrapper #} c s

-- |Set the local origin of the `Circle'.
setOrigin :: Circle -> Vector2D Float -> IO ()
setOrigin circle origin =
    withCircle circle $ \c ->
    with (toVector2F origin) $ \o ->
    {# call unsafe sfCircleShape_setOrigin_wrapper #} c o

-- |Get the position of the `Circle'
getPosition :: Circle -> IO (Vector2D Float)
getPosition circle =
    withCircle circle $ \c ->
    alloca $ \position ->
    {# call unsafe sfCircleShape_getPosition_wrapper #} c position >>
    fromVector2F <$> peek position

-- |Get the orientation of the `Circle'
getRotation :: Circle -> IO Float
getRotation circle =
    withCircle circle $ \c ->
    {# call unsafe sfCircleShape_getRotation #} c >>= \res ->
    return $ cFloatConv res

-- |Get the current scale of the `Circle'
getScale :: Circle -> IO (Vector2D Float)
getScale circle =
    withCircle circle $ \c ->
    alloca $ \scales ->
    {# call unsafe sfCircleShape_getScale_wrapper #} c scales >>
    fromVector2F <$> peek scales

-- |Get the local origin of the `Circle'
getOrigin :: Circle -> IO (Vector2D Float)
getOrigin circle =
    withCircle circle $ \c ->
    alloca $ \origin ->
    {# call unsafe sfCircleShape_getOrigin_wrapper #} c origin >>
    fromVector2F <$> peek origin

-- |Move the `Circle' by a given offset. This function adds to the current position of the object.
move :: Circle -> Vector2D Float -> IO ()
move circle offset =
    withCircle circle $ \c ->
    with (toVector2F offset) $ \o ->
    {# call unsafe sfCircleShape_move_wrapper #} c o

-- |Rotate the `Circle'. This function adds to the current rotation of the object.
rotate :: Circle -> Float -> IO ()
rotate circle angle =
    withCircle circle $ \c ->
    {# call unsafe sfCircleShape_rotate #} c (cFloatConv angle)

-- |Scale the `Circle'. This function multiplies the current scale of the object.
scale :: Circle -> Vector2D Float -> IO ()
scale circle factors =
    withCircle circle $ \c ->
    with (toVector2F factors) $ \f ->
    {# call unsafe sfCircleShape_scale_wrapper #} c f

-- |Get the combined transform of the `Circle'
getTransform :: Circle -> IO Transform
getTransform circle =
    withCircle circle $ \c ->
    alloca $ \transform ->
    {# call unsafe sfCircleShape_getTransform_wrapper #} c transform >>
    peek transform

-- |Get the inverse of the combined transform of the `Circle'.
getInverseTransform :: Circle -> IO Transform
getInverseTransform circle =
    withCircle circle $ \c ->
    alloca $ \transform ->
    {# call unsafe sfCircleShape_getInverseTransform_wrapper #} c transform >>
    peek transform

-- |Change the source `Texture' of the `Circle'. The texture must exist as long as the circle uses it.
setTexture :: Circle -> Maybe Texture -> Bool -> IO ()
setTexture circle@(Circle _ textureRef) texture reset = 
    withCircle circle $ \c ->
    writeIORef textureRef texture >>
    case texture of
        Nothing -> {# call unsafe sfCircleShape_setTexture #} c nullPtr (cIntFromBool reset)
        Just t  -> withTexture t $ \t' -> 
                   {# call unsafe sfCircleShape_setTexture #} c t' (cIntFromBool reset)

-- |Set the sub-rectangle of the `Texture' that the `Circle' will display. 
setTextureRect :: Circle -> Rect Int -> IO ()
setTextureRect circle rect =
    withCircle circle $ \c ->
    with (toIntRect rect) $ \r ->
    {# call unsafe sfCircleShape_setTextureRect_wrapper #} c r

-- |Set the fill `Color' of the `Circle'. Default is opaque white.
setFillColor :: Circle -> Color -> IO ()
setFillColor circle color =
    withCircle circle $ \c ->
    with color $ \col ->
    {# call unsafe sfCircleShape_setFillColor_wrapper #} c col

-- |Set the outline `Color' of the `Circle'. Default is opaque white.
setOutlineColor :: Circle -> Color -> IO ()
setOutlineColor circle color =
    withCircle circle $ \c ->
    with color $ \col ->
    {# call unsafe sfCircleShape_setOutlineColor_wrapper #} c col

-- |Set the thickness of the `Circle''s outline. Default is 0.
setOutlineThickness :: Circle -> Float -> IO ()
setOutlineThickness circle thickness =
    withCircle circle $ \c ->
    {# call unsafe sfCircleShape_setOutlineThickness #} c (cFloatConv thickness)

--FIXME: Get value from IORef ?
-- |Get the source `Texture' of the `Circle'.
getTexture :: Circle -> IO (Maybe Texture)
getTexture circle =
    withCircle circle $ \c ->
    {# call unsafe sfCircleShape_getTexture #} c >>= \res ->
    ptrToMaybe mkConstTexture res

-- |Get the sub-rectangle of the `Texture' displayed by the `Circle'.
getTextureRect :: Circle -> IO (Rect Int)
getTextureRect circle =
    withCircle circle $ \c ->
    alloca $ \rect ->
    {# call unsafe sfCircleShape_getTextureRect_wrapper #} c rect >>
    fromIntRect <$> peek rect

-- |Get the fill `Color' of the `Circle'.
getFillColor :: Circle -> IO Color
getFillColor circle =
    withCircle circle $ \c ->
    alloca $ \color ->
    {# call unsafe sfCircleShape_getFillColor_wrapper #} c color >>
    peek color

-- |Get the outline `Color' of the `Circle'.
getOutlineColor :: Circle -> IO Color
getOutlineColor circle =
    withCircle circle $ \c ->
    alloca $ \color ->
    {# call unsafe sfCircleShape_getOutlineColor_wrapper #} c color >>
    peek color

-- |Get the outline thickness of the `Circle'.
getOutlineThickness :: Circle -> IO Float
getOutlineThickness circle =
    withCircle circle $ \c ->
    {# call unsafe sfCircleShape_getOutlineThickness #} c >>= \res ->
    return $ cFloatConv res

-- |Get the number of points of the `Circle'.
getPointCount :: Circle -> IO Word
getPointCount circle =
    withCircle circle $ \c ->
    {# call unsafe sfCircleShape_getPointCount #} c >>= \res ->
    return $ cIntConv res

-- |Get a point of the `Circle'.
getPoint :: Circle -> Word -> IO (Vector2D Float)
getPoint circle index =
    withCircle circle $ \c ->
    alloca $ \point ->
    {# call unsafe sfCircleShape_getPoint_wrapper #} c (cIntConv index) point >>
    fromVector2F <$> peek point

-- |Set the radius of the `Circle'.
setRadius :: Circle -> Float -> IO ()
setRadius circle radius =
    withCircle circle $ \c ->
    {# call unsafe sfCircleShape_setRadius #} c (cFloatConv radius)

-- |Get the radius of the `Circle'.
getRadius :: Circle -> IO Float
getRadius circle =
    withCircle circle $ \c ->
    {# call unsafe sfCircleShape_getRadius #} c >>= \res ->
    return $ cFloatConv res

-- |Set the number of points of the `Circle'.
setPointCount :: Circle -> Word -> IO ()
setPointCount circle count =
    withCircle circle $ \c ->
    {# call unsafe sfCircleShape_setPointCount #} c (cIntConv count)

-- |Get the local bounding rectangle of the `Circle'.
getLocalBounds :: Circle -> IO (Rect Float)
getLocalBounds circle =
    withCircle circle $ \c ->
    alloca $ \rect ->
    {# call unsafe sfCircleShape_getLocalBounds_wrapper #} c rect >>
    fromFloatRect <$> peek rect

-- |Get the global bounding rectangle of the `Circle'.
getGlobalBounds :: Circle -> IO (Rect Float)
getGlobalBounds circle =
    withCircle circle $ \c ->
    alloca $ \rect ->
    {# call unsafe sfCircleShape_getGlobalBounds_wrapper #} c rect >>
    fromFloatRect <$> peek rect

