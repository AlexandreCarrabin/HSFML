{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.Convex where


import Control.Applicative ((<$>), (<*>))
import Data.IORef (newIORef, writeIORef)
import Data.Word (Word)
import Foreign (FinalizerPtr, alloca, newForeignPtr, nullPtr, peek, with)
import Foreign.C.Types (CFloat, CInt, CUInt)

{# import SFML.Graphics.Internal.Rect #} (FloatRectPtr, IntRectPtr, Rect, fromFloatRect, fromIntRect, toIntRect)
{# import SFML.Graphics.Internal.Texture #} (mkConstTexture)
{# import SFML.Graphics.Internal.Transform #} (Transform, TransformPtr)
{# import SFML.Graphics.Internal.Color #} (Color, ColorPtr)
{# import SFML.Graphics.Internal.Types #} (CSFML_Convex, Convex(..), ConvexPtr, withConvex, Texture, TexturePtr, withTexture)

{# import SFML.System.Internal.Vector2 #} (Vector2D, Vector2FPtr, fromVector2F, toVector2F)

import SFML.Utility.Foreign (cFloatConv, cIntConv, cIntFromBool, ptrToMaybe)

#include <SFML/Graphics/ConvexShape.h>
#include <SFML/Graphics/ConvexShapeWrapper.h>


-- |Create a new `Convex'.
createConvex :: IO Convex
createConvex =
    {# call unsafe sfConvexShape_create #} >>= \res ->
    mkConvex res

mkConvex :: ConvexPtr -> IO Convex
mkConvex ptr = Convex <$> newForeignPtr c_destroyConvex ptr <*> newIORef Nothing

-- |Create a new `Convex' by copying an existing one.
copyConvex :: Convex -> IO Convex
copyConvex convex =
    withConvex convex $ \c ->
    {# call unsafe sfConvexShape_copy #} c >>= \copy ->
    getTexture convex >>= \texture ->
    Convex <$> newForeignPtr c_destroyConvex copy <*> newIORef texture

-- |Destroy a `Convex'.
foreign import ccall unsafe "SFML/Graphics/ConvexShape.h &sfConvexShape_destroy"
    c_destroyConvex :: FinalizerPtr CSFML_Convex

-- |Set the position of the `Convex'. This function completely overwrites the previous position.
setPosition :: Convex -> Vector2D Float -> IO ()
setPosition convex position =
    withConvex convex $ \c ->
    with (toVector2F position) $ \p ->
    {# call unsafe sfConvexShape_setPosition_wrapper #} c p

-- |Set the orientation of the `Convex'. This function completely overwrites the previous rotation.
setRotation :: Convex -> Float -> IO ()
setRotation convex angle =
    withConvex convex $ \c ->
    {# call unsafe sfConvexShape_setRotation #} c (cFloatConv angle)

-- |Set the scale factors of the `Convex'. This function completely overwrites the previous scale.
setScale :: Convex -> Vector2D Float -> IO ()
setScale convex scales =
    withConvex convex $ \c ->
    with (toVector2F scales) $ \s ->
    {# call unsafe sfConvexShape_setScale_wrapper #} c s

-- |Set the local origin of the `Convex'.
setOrigin :: Convex -> Vector2D Float -> IO ()
setOrigin convex origin =
    withConvex convex $ \c ->
    with (toVector2F origin) $ \o ->
    {# call unsafe sfConvexShape_setOrigin_wrapper #} c o

-- |Get the position of the `Convex'
getPosition :: Convex -> IO (Vector2D Float)
getPosition convex =
    withConvex convex $ \c ->
    alloca $ \position ->
    {# call unsafe sfConvexShape_getPosition_wrapper #} c position >>
    fromVector2F <$> peek position

-- |Get the orientation of the `Convex'
getRotation :: Convex -> IO Float
getRotation convex =
    withConvex convex $ \c ->
    {# call unsafe sfConvexShape_getRotation #} c >>= \res ->
    return $ cFloatConv res

-- |Get the current scale of the `Convex'
getScale :: Convex -> IO (Vector2D Float)
getScale convex =
    withConvex convex $ \c ->
    alloca $ \scales ->
    {# call unsafe sfConvexShape_getScale_wrapper #} c scales >>
    fromVector2F <$> peek scales

-- |Get the local origin of the `Convex'
getOrigin :: Convex -> IO (Vector2D Float)
getOrigin convex =
    withConvex convex $ \c ->
    alloca $ \origin ->
    {# call unsafe sfConvexShape_getOrigin_wrapper #} c origin >>
    fromVector2F <$> peek origin

-- |Move the `Convex' by a given offset. This function adds to the current position of the object.
move :: Convex -> Vector2D Float -> IO ()
move convex offset =
    withConvex convex $ \c ->
    with (toVector2F offset) $ \o ->
    {# call unsafe sfConvexShape_move_wrapper #} c o

-- |Rotate the `Convex'. This function adds to the current rotation of the object.
rotate :: Convex -> Float -> IO ()
rotate convex angle =
    withConvex convex $ \c ->
    {# call unsafe sfConvexShape_rotate #} c (cFloatConv angle)

-- |Scale the `Convex'. This function multiplies the current scale of the object.
scale :: Convex -> Vector2D Float -> IO ()
scale convex factors =
    withConvex convex $ \c ->
    with (toVector2F factors) $ \f ->
    {# call unsafe sfConvexShape_scale_wrapper #} c f

-- |Get the combined transform of the `Convex'
getTransform :: Convex -> IO Transform
getTransform convex =
    withConvex convex $ \c ->
    alloca $ \transform ->
    {# call unsafe sfConvexShape_getTransform_wrapper #} c transform >>
    peek transform

-- |Get the inverse of the combined transform of the `Convex'.
getInverseTransform :: Convex -> IO Transform
getInverseTransform convex =
    withConvex convex $ \c ->
    alloca $ \transform ->
    {# call unsafe sfConvexShape_getInverseTransform_wrapper #} c transform >>
    peek transform

-- |Change the source `Texture' of the `Convex'. The texture must exist as long as the convex uses it.
setTexture :: Convex -> Maybe Texture -> Bool -> IO ()
setTexture convex@(Convex _ textureRef) texture reset = 
    withConvex convex $ \c ->
    writeIORef textureRef texture >>
    case texture of
        Nothing -> {# call unsafe sfConvexShape_setTexture #} c nullPtr (cIntFromBool reset)
        Just t  -> withTexture t $ \t' -> 
                   {# call unsafe sfConvexShape_setTexture #} c t' (cIntFromBool reset)

-- |Set the sub-rectangle of the `Texture' that the `Convex' will display. 
setTextureRect :: Convex -> Rect Int -> IO ()
setTextureRect convex rect =
    withConvex convex $ \c ->
    with (toIntRect rect) $ \r ->
    {# call unsafe sfConvexShape_setTextureRect_wrapper #} c r

-- |Set the fill `Color' of the `Convex'. Default is opaque white.
setFillColor :: Convex -> Color -> IO ()
setFillColor convex color =
    withConvex convex $ \c ->
    with color $ \col ->
    {# call unsafe sfConvexShape_setFillColor_wrapper #} c col

-- |Set the outline `Color' of the `Convex'. Default is opaque white.
setOutlineColor :: Convex -> Color -> IO ()
setOutlineColor convex color =
    withConvex convex $ \c ->
    with color $ \col ->
    {# call unsafe sfConvexShape_setOutlineColor_wrapper #} c col

-- |Set the thickness of the `Convex''s outline. Default is 0.
setOutlineThickness :: Convex -> Float -> IO ()
setOutlineThickness convex thickness =
    withConvex convex $ \c ->
    {# call unsafe sfConvexShape_setOutlineThickness #} c (cFloatConv thickness)

--FIXME: Get value from IORef ?
-- |Get the source `Texture' of the `Convex'.
getTexture :: Convex -> IO (Maybe Texture)
getTexture convex =
    withConvex convex $ \c ->
    {# call unsafe sfConvexShape_getTexture #} c >>= \res ->
    ptrToMaybe mkConstTexture res

-- |Get the sub-rectangle of the `Texture' displayed by the `Convex'.
getTextureRect :: Convex -> IO (Rect Int)
getTextureRect convex =
    withConvex convex $ \c ->
    alloca $ \rect ->
    {# call unsafe sfConvexShape_getTextureRect_wrapper #} c rect >>
    fromIntRect <$> peek rect

-- |Get the fill `Color' of the `Convex'.
getFillColor :: Convex -> IO Color
getFillColor convex =
    withConvex convex $ \c ->
    alloca $ \color ->
    {# call unsafe sfConvexShape_getFillColor_wrapper #} c color >>
    peek color

-- |Get the outline `Color' of the `Convex'.
getOutlineColor :: Convex -> IO Color
getOutlineColor convex =
    withConvex convex $ \c ->
    alloca $ \color ->
    {# call unsafe sfConvexShape_getOutlineColor_wrapper #} c color >>
    peek color

-- |Get the outline thickness of the `Convex'.
getOutlineThickness :: Convex -> IO Float
getOutlineThickness convex =
    withConvex convex $ \c ->
    {# call unsafe sfConvexShape_getOutlineThickness #} c >>= \res ->
    return $ cFloatConv res

--FIXME: Remove unsafe ?
-- |Get the number of points of the `Convex'.
getPointCount :: Convex -> IO Word
getPointCount convex =
    withConvex convex $ \c ->
    {# call unsafe sfConvexShape_getPointCount #} c >>= \res ->
    return $ cIntConv res

--FIXME: Remove unsafe ?
-- |Get a point of the `Convex'.
getPoint :: Convex -> Word -> IO (Vector2D Float)
getPoint convex index =
    withConvex convex $ \c ->
    alloca $ \point ->
    {# call unsafe sfConvexShape_getPoint_wrapper #} c (cIntConv index) point >>
    fromVector2F <$> peek point

-- |Set the number of points of the `Convex'.
setPointCount :: Convex -> Word -> IO ()
setPointCount convex count =
    withConvex convex $ \c ->
    {# call unsafe sfConvexShape_setPointCount #} c (cIntConv count)

-- |Set the position of a point.
setPoint :: Convex -> Word -> Vector2D Float -> IO ()
setPoint convex index point =
    withConvex convex $ \c ->
    with (toVector2F point) $ \p ->
    {# call unsafe sfConvexShape_setPoint_wrapper #} c (cIntConv index) p

-- |Get the local bounding rectangle of the `Convex'.
getLocalBounds :: Convex -> IO (Rect Float)
getLocalBounds convex =
    withConvex convex $ \c ->
    alloca $ \rect ->
    {# call unsafe sfConvexShape_getLocalBounds_wrapper #} c rect >>
    fromFloatRect <$> peek rect

-- |Get the global bounding rectangle of the `Convex'.
getGlobalBounds :: Convex -> IO (Rect Float)
getGlobalBounds convex =
    withConvex convex $ \c ->
    alloca $ \rect ->
    {# call unsafe sfConvexShape_getGlobalBounds_wrapper #} c rect >>
    fromFloatRect <$> peek rect

