{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.Rectangle where


import Control.Applicative ((<$>), (<*>))
import Data.Word (Word)
import Data.IORef (newIORef, writeIORef)
import Foreign (FinalizerPtr, alloca, newForeignPtr, nullPtr, peek, with)
import Foreign.C.Types (CFloat, CInt, CUInt)

{# import SFML.Graphics.Internal.Rect #} (FloatRectPtr, IntRectPtr , Rect, fromFloatRect, fromIntRect, toIntRect)
{# import SFML.Graphics.Internal.Texture #} (mkConstTexture)
{# import SFML.Graphics.Internal.Transform #} (Transform, TransformPtr)
{# import SFML.Graphics.Internal.Color #} (Color, ColorPtr)
{# import SFML.Graphics.Internal.Types #} (Rectangle(..), RectanglePtr, CSFML_Rectangle, withRectangle , Texture, TexturePtr, withTexture)

{# import SFML.System.Internal.Vector2 #} (Vector2D, Vector2FPtr, fromVector2F, toVector2F)

import SFML.Utility.Foreign (cFloatConv, cIntConv, cIntFromBool, ptrToMaybe)

#include <SFML/Graphics/RectangleShape.h>
#include <SFML/Graphics/RectangleShapeWrapper.h>


-- |Create a new `Rectangle'.
createRectangle :: IO Rectangle
createRectangle =
    {# call unsafe sfRectangleShape_create #} >>= \res ->
    mkRectangle res

mkRectangle :: RectanglePtr -> IO Rectangle
mkRectangle ptr = Rectangle <$> newForeignPtr c_destroyRectangle ptr <*> newIORef Nothing

-- |Create a new `Rectangle' by copying an existing one.
copyRectangle :: Rectangle -> IO Rectangle
copyRectangle rectangle =
    withRectangle rectangle $ \r ->
    {# call unsafe sfRectangleShape_copy #} r >>= \copy ->
    getTexture rectangle >>= \texture ->
    Rectangle <$> newForeignPtr c_destroyRectangle copy <*> newIORef texture

-- |Destroy a `Rectangle'.
foreign import ccall unsafe "SFML/Graphics/RectangleShape.h &sfRectangleShape_destroy"
    c_destroyRectangle :: FinalizerPtr CSFML_Rectangle

-- |Set the position of the `Rectangle'. This function completely overwrites the previous position.
setPosition :: Rectangle -> Vector2D Float -> IO ()
setPosition rectangle position =
    withRectangle rectangle $ \r ->
    with (toVector2F position) $ \p ->
    {# call unsafe sfRectangleShape_setPosition_wrapper #} r p

-- |Set the orientation of the `Rectangle'. This function completely overwrites the previous rotation.
setRotation :: Rectangle -> Float -> IO ()
setRotation rectangle angle =
    withRectangle rectangle $ \r ->
    {# call unsafe sfRectangleShape_setRotation #} r (cFloatConv angle)

-- |Set the scale factors of the `Rectangle'. This function completely overwrites the previous scale.
setScale :: Rectangle -> Vector2D Float -> IO ()
setScale rectangle scales =
    withRectangle rectangle $ \r ->
    with (toVector2F scales) $ \s ->
    {# call unsafe sfRectangleShape_setScale_wrapper #} r s

-- |Set the local origin of the `Rectangle'.
setOrigin :: Rectangle -> Vector2D Float -> IO ()
setOrigin rectangle origin =
    withRectangle rectangle $ \r ->
    with (toVector2F origin) $ \o ->
    {# call unsafe sfRectangleShape_setOrigin_wrapper #} r o

-- |Get the position of the `Rectangle'
getPosition :: Rectangle -> IO (Vector2D Float)
getPosition rectangle =
    withRectangle rectangle $ \r ->
    alloca $ \position ->
    {# call unsafe sfRectangleShape_getPosition_wrapper #} r position >>
    fromVector2F <$> peek position

-- |Get the orientation of the `Rectangle'
getRotation :: Rectangle -> IO Float
getRotation rectangle =
    withRectangle rectangle $ \r ->
    {# call unsafe sfRectangleShape_getRotation #} r >>= \res ->
    return $ cFloatConv res

-- |Get the current scale of the `Rectangle'
getScale :: Rectangle -> IO (Vector2D Float)
getScale rectangle =
    withRectangle rectangle $ \r ->
    alloca $ \scales ->
    {# call unsafe sfRectangleShape_getScale_wrapper #} r scales >>
    fromVector2F <$> peek scales

-- |Get the local origin of the `Rectangle'
getOrigin :: Rectangle -> IO (Vector2D Float)
getOrigin rectangle =
    withRectangle rectangle $ \r ->
    alloca $ \origin ->
    {# call unsafe sfRectangleShape_getOrigin_wrapper #} r origin >>
    fromVector2F <$> peek origin

-- |Move the `Rectangle' by a given offset. This function adds to the current position of the object.
move :: Rectangle -> Vector2D Float -> IO ()
move rectangle offset =
    withRectangle rectangle $ \r ->
    with (toVector2F offset) $ \o ->
    {# call unsafe sfRectangleShape_move_wrapper #} r o

-- |Rotate the `Rectangle'. This function adds to the current rotation of the object.
rotate :: Rectangle -> Float -> IO ()
rotate rectangle angle =
    withRectangle rectangle $ \r ->
    {# call unsafe sfRectangleShape_rotate #} r (cFloatConv angle)

-- |Scale the `Rectangle'. This function multiplies the current scale of the object.
scale :: Rectangle -> Vector2D Float -> IO ()
scale rectangle factors =
    withRectangle rectangle $ \r ->
    with (toVector2F factors) $ \f ->
    {# call unsafe sfRectangleShape_scale_wrapper #} r f

-- |Get the combined transform of the `Rectangle'
getTransform :: Rectangle -> IO Transform
getTransform rectangle =
    withRectangle rectangle $ \r ->
    alloca $ \transform ->
    {# call unsafe sfRectangleShape_getTransform_wrapper #} r transform >>
    peek transform

-- |Get the inverse of the combined transform of the `Rectangle'.
getInverseTransform :: Rectangle -> IO Transform
getInverseTransform rectangle =
    withRectangle rectangle $ \r ->
    alloca $ \transform ->
    {# call unsafe sfRectangleShape_getInverseTransform_wrapper #} r transform >>
    peek transform

-- |Change the source `Texture' of the `Rectangle'. The texture must exist as long as the rectangle uses it.
setTexture :: Rectangle -> Maybe Texture -> Bool -> IO ()
setTexture rectangle@(Rectangle _ textureRef) texture reset = 
    withRectangle rectangle $ \r ->
    writeIORef textureRef texture >>
    case texture of
        Nothing -> {# call unsafe sfRectangleShape_setTexture #} r nullPtr (cIntFromBool reset)
        Just t  -> withTexture t $ \t' -> 
                   {# call unsafe sfRectangleShape_setTexture #} r t' (cIntFromBool reset)

-- |Set the sub-rectangle of the `Texture' that the `Rectangle' will display. 
setTextureRect :: Rectangle -> Rect Int -> IO ()
setTextureRect rectangle rect =
    withRectangle rectangle $ \r ->
    with (toIntRect rect) $ \area ->
    {# call unsafe sfRectangleShape_setTextureRect_wrapper #} r area

-- |Set the fill `Color' of the `Rectangle'. Default is opaque white.
setFillColor :: Rectangle -> Color -> IO ()
setFillColor rectangle color =
    withRectangle rectangle $ \r ->
    with color $ \c ->
    {# call unsafe sfRectangleShape_setFillColor_wrapper #} r c

-- |Set the outline `Color' of the `Rectangle'. Default is opaque white.
setOutlineColor :: Rectangle -> Color -> IO ()
setOutlineColor rectangle color =
    withRectangle rectangle $ \r ->
    with color $ \c ->
    {# call unsafe sfRectangleShape_setOutlineColor_wrapper #} r c

-- |Set the thickness of the `Rectangle''s outline. Default is 0.
setOutlineThickness :: Rectangle -> Float -> IO ()
setOutlineThickness rectangle thickness =
    withRectangle rectangle $ \r ->
    {# call unsafe sfRectangleShape_setOutlineThickness #} r (cFloatConv thickness)

--FIXME: Get value from IORef ?
-- |Get the source `Texture' of the `Rectangle'.
getTexture :: Rectangle -> IO (Maybe Texture)
getTexture rectangle =
    withRectangle rectangle $ \r ->
    {# call unsafe sfRectangleShape_getTexture #} r >>= \res ->
    ptrToMaybe mkConstTexture res

-- |Get the sub-rectangle of the `Texture' displayed by the `Rectangle'.
getTextureRect :: Rectangle -> IO (Rect Int)
getTextureRect rectangle =
    withRectangle rectangle $ \r ->
    alloca $ \rect ->
    {# call unsafe sfRectangleShape_getTextureRect_wrapper #} r rect >>
    fromIntRect <$> peek rect

-- |Get the fill `Color' of the `Rectangle'.
getFillColor :: Rectangle -> IO Color
getFillColor rectangle =
    withRectangle rectangle $ \r ->
    alloca $ \color ->
    {# call unsafe sfRectangleShape_getFillColor_wrapper #} r color >>
    peek color

-- |Get the outline `Color' of the `Rectangle'.
getOutlineColor :: Rectangle -> IO Color
getOutlineColor rectangle =
    withRectangle rectangle $ \r ->
    alloca $ \color ->
    {# call unsafe sfRectangleShape_getOutlineColor_wrapper #} r color >>
    peek color

-- |Get the outline thickness of the `Rectangle'.
getOutlineThickness :: Rectangle -> IO Float
getOutlineThickness rectangle =
    withRectangle rectangle $ \r ->
    {# call unsafe sfRectangleShape_getOutlineThickness #} r >>= \res ->
    return $ cFloatConv res

--FIXME: Remove unsafe ?
-- |Get the number of points of the `Rectangle'.
getPointCount :: Rectangle -> IO Word
getPointCount rectangle =
    withRectangle rectangle $ \r ->
    {# call unsafe sfRectangleShape_getPointCount #} r >>= \res ->
    return $ cIntConv res

--FIXME: Remove unsafe ?
-- |Get a point of the `Rectangle'.
getPoint :: Rectangle -> Word -> IO (Vector2D Float)
getPoint rectangle index =
    withRectangle rectangle $ \r ->
    alloca $ \point ->
    {# call unsafe sfRectangleShape_getPoint_wrapper #} r (cIntConv index) point >>
    fromVector2F <$> peek point

-- |Set the size (width and height) of the `Rectangle'.
setSize :: Rectangle -> Vector2D Float -> IO ()
setSize rectangle size =
    withRectangle rectangle $ \r ->
    with (toVector2F size) $ \s ->
    {# call unsafe sfRectangleShape_setSize_wrapper #} r s

-- |Get the size of the `Rectangle'.
getSize :: Rectangle -> IO (Vector2D Float)
getSize rectangle =
    withRectangle rectangle $ \r ->
    alloca $ \size ->
    {# call unsafe sfRectangleShape_getSize_wrapper #} r size >>
    fromVector2F <$> peek size

-- |Get the local bounding rectangle of the `Rectangle'.
getLocalBounds :: Rectangle -> IO (Rect Float)
getLocalBounds rectangle =
    withRectangle rectangle $ \r ->
    alloca $ \rect ->
    {# call unsafe sfRectangleShape_getLocalBounds_wrapper #} r rect >>
    fromFloatRect <$> peek rect

-- |Get the global bounding rectangle of the `Rectangle'.
getGlobalBounds :: Rectangle -> IO (Rect Float)
getGlobalBounds rectangle =
    withRectangle rectangle $ \r ->
    alloca $ \rect ->
    {# call unsafe sfRectangleShape_getGlobalBounds_wrapper #} r rect >>
    fromFloatRect <$> peek rect

