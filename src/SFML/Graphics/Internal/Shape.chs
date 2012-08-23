{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.Shape where


import Control.Applicative ((<$>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word)
import Foreign (Ptr, alloca, nullPtr, peek, poke, with)
import Foreign.C.Types (CFloat, CInt, CUInt)
import Foreign.Concurrent (newForeignPtr)
import Foreign.StablePtr (StablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)

{# import SFML.Graphics.Internal.Rect #} ( FloatRectPtr, IntRectPtr , Rect(..), fromFloatRect, fromIntRect, toIntRect)
{# import SFML.Graphics.Internal.Texture #} (mkConstTexture)
{# import SFML.Graphics.Internal.Transform #} (Transform, TransformPtr)
{# import SFML.Graphics.Internal.Color #} (Color, ColorPtr)
{# import SFML.Graphics.Internal.Types #} (Shape(..), ShapePtr, ShapeCallbackData(..), withShape, Texture, TexturePtr, withTexture)

{# import SFML.System.Internal.Vector2 #} (Vector2D, Vector2FPtr, fromVector2F, toVector2F)

import SFML.Utility.Foreign (cFloatConv, cIntConv, cIntFromBool, ptrToMaybe)

#include <SFML/Graphics/Shape.h>
#include <SFML/Graphics/ShapeWrapper.h>


-- |Callback to get the number of points of the `Shape' from the data.
type CallbackGetPointCount a = a -> Word
-- |Callback to get a given point of the `Shape' from the data.
type CallbackGetPoint a = a -> Word -> Vector2D Float

createShapeCallbackData :: a -> CallbackGetPointCount a -> CallbackGetPoint a -> IO (StablePtr (IORef (ShapeCallbackData a)))
createShapeCallbackData d callbackGetPointCount callbackGetPoint =
    newStablePtr =<< newIORef (ShapeCallbackData d callbackGetPointCount callbackGetPoint)

readShapeCallbackData :: StablePtr (IORef (ShapeCallbackData a)) -> IO (ShapeCallbackData a)
readShapeCallbackData ptr = readIORef =<< deRefStablePtr ptr

writeShapeCallbackData :: StablePtr (IORef (ShapeCallbackData a)) -> ShapeCallbackData a -> IO ()
writeShapeCallbackData ptr value = deRefStablePtr ptr >>= flip writeIORef value

foreign export ccall hs_getPointCountCallback :: StablePtr (IORef (ShapeCallbackData a)) -> IO CUInt
hs_getPointCountCallback :: StablePtr (IORef (ShapeCallbackData a)) -> IO CUInt
hs_getPointCountCallback value = 
    readShapeCallbackData value >>= \(ShapeCallbackData d callbackGetPointCount _) ->
    return $ (cIntConv . callbackGetPointCount) d

foreign export ccall hs_getPointCallback :: StablePtr (IORef (ShapeCallbackData a)) -> CUInt -> Vector2FPtr -> IO ()
hs_getPointCallback :: StablePtr (IORef (ShapeCallbackData a)) -> CUInt -> Vector2FPtr -> IO ()
hs_getPointCallback value index point =
    readShapeCallbackData value >>= \(ShapeCallbackData d _ callbackGetPoint) ->
    poke point $ toVector2F (callbackGetPoint d (cIntConv index))


--FIXME: Remove unsafe ? (only to update ?)
-- |Create a new `Shape' with the given data and two callbacks.
createShape :: a -> CallbackGetPointCount a -> CallbackGetPoint a -> IO (Shape a)
createShape d callbackGetPointCount callbackGetPoint = do
    texture <- newIORef Nothing
    dataPtr <- createShapeCallbackData d callbackGetPointCount callbackGetPoint
    c_shape <- {# call unsafe sfShape_create_wrapper #} $ castStablePtrToPtr dataPtr
    --c_shape <- {# call sfShape_create_wrapper #} $ castStablePtrToPtr dataPtr
    shapePtr <- newForeignPtr c_shape (finalizeShape c_shape dataPtr)
    --{# call unsafe sfShape_update #} c_shape
    {# call sfShape_update #} c_shape
    return $ Shape shapePtr texture dataPtr

-- |Destroy a `Shape'.
finalizeShape :: ShapePtr -> StablePtr (IORef (ShapeCallbackData a)) -> IO ()
finalizeShape shape callbackData = do
    {# call unsafe sfShape_destroy #} shape
    freeStablePtr callbackData

-- |Set the position of the `Shape'. This function completely overwrites the previous position.
setPosition :: Shape a -> Vector2D Float -> IO ()
setPosition shape position =
    withShape shape $ \s ->
    with (toVector2F position) $ \p ->
    {# call unsafe sfShape_setPosition_wrapper #} s p

-- |Set the orientation of the `Shape'. This function completely overwrites the previous rotation.
setRotation :: Shape a -> Float -> IO ()
setRotation shape angle =
    withShape shape $ \s ->
    {# call unsafe sfShape_setRotation #} s (cFloatConv angle)

-- |Set the scale factors of the `Shape'. This function completely overwrites the previous scale.
setScale :: Shape a -> Vector2D Float -> IO ()
setScale shape scales =
    withShape shape $ \sh ->
    with (toVector2F scales) $ \sc ->
    {# call unsafe sfShape_setScale_wrapper #} sh sc

-- |Set the local origin of the `Shape'.
setOrigin :: Shape a -> Vector2D Float -> IO ()
setOrigin shape origin =
    withShape shape $ \s ->
    with (toVector2F origin) $ \o ->
    {# call unsafe sfShape_setOrigin_wrapper #} s o

-- |Get the position of the `Shape'
getPosition :: Shape a -> IO (Vector2D Float)
getPosition shape =
    withShape shape $ \s ->
    alloca $ \position ->
    {# call unsafe sfShape_getPosition_wrapper #} s position >>
    fromVector2F <$> peek position

-- |Get the orientation of the `Shape'
getRotation :: Shape a -> IO Float
getRotation shape =
    withShape shape $ \s ->
    {# call unsafe sfShape_getRotation #} s >>= \res ->
    return $ cFloatConv res

-- |Get the current scale of the `Shape'
getScale :: Shape a -> IO (Vector2D Float)
getScale shape =
    withShape shape $ \s ->
    alloca $ \scales ->
    {# call unsafe sfShape_getScale_wrapper #} s scales >>
    fromVector2F <$> peek scales

-- |Get the local origin of the `Shape'
getOrigin :: Shape a -> IO (Vector2D Float)
getOrigin shape =
    withShape shape $ \s ->
    alloca $ \origin ->
    {# call unsafe sfShape_getOrigin_wrapper #} s origin >>
    fromVector2F <$> peek origin

-- |Move the `Shape' by a given offset. This function adds to the current position of the object.
move :: Shape a -> Vector2D Float -> IO ()
move shape offset =
    withShape shape $ \s ->
    with (toVector2F offset) $ \o ->
    {# call unsafe sfShape_move_wrapper #} s o

-- |Rotate the `Shape'. This function adds to the current rotation of the object.
rotate :: Shape a -> Float -> IO ()
rotate shape angle =
    withShape shape $ \s ->
    {# call unsafe sfShape_rotate #} s (cFloatConv angle)

-- |Scale the `Shape'. This function multiplies the current scale of the object.
scale :: Shape a -> Vector2D Float -> IO ()
scale shape factors =
    withShape shape $ \s ->
    with (toVector2F factors) $ \f ->
    {# call unsafe sfShape_scale_wrapper #} s f

-- |Get the combined transform of the `Shape'
getTransform :: Shape a -> IO Transform
getTransform shape =
    withShape shape $ \s ->
    alloca $ \transform ->
    {# call unsafe sfShape_getTransform_wrapper #} s transform >>
    peek transform

-- |Get the inverse of the combined transform of the `Shape'.
getInverseTransform :: Shape a -> IO Transform
getInverseTransform shape =
    withShape shape $ \s ->
    alloca $ \transform ->
    {# call unsafe sfShape_getInverseTransform_wrapper #} s transform >>
    peek transform

-- |Change the source `Texture' of the `Shape'. The texture must exist as long as the convex uses it.
setTexture :: Shape a -> Maybe Texture -> Bool -> IO ()
setTexture shape@(Shape _ textureRef _) texture reset = 
    withShape shape $ \s ->
    writeIORef textureRef texture >>
    case texture of
        Nothing -> {# call unsafe sfShape_setTexture #} s nullPtr (cIntFromBool reset)
        Just t  -> withTexture t $ \t' -> 
                   {# call unsafe sfShape_setTexture #} s t' (cIntFromBool reset)

-- |Set the sub-rectangle of the `Texture' that the `Shape' will display. 
setTextureRect :: Shape a -> Rect Int -> IO ()
setTextureRect shape rect =
    withShape shape $ \s ->
    with (toIntRect rect) $ \r ->
    {# call unsafe sfShape_setTextureRect_wrapper #} s r

-- |Set the fill `Color' of the `Shape'. Default is opaque white.
setFillColor :: Shape a -> Color -> IO ()
setFillColor shape color =
    withShape shape $ \s ->
    with color $ \c ->
    {# call unsafe sfShape_setFillColor_wrapper #} s c

-- |Set the outline `Color' of the `Shape'. Default is opaque white.
setOutlineColor :: Shape a -> Color -> IO ()
setOutlineColor shape color =
    withShape shape $ \s ->
    with color $ \c ->
    {# call unsafe sfShape_setOutlineColor_wrapper #} s c

-- |Set the thickness of the `Shape''s outline. Default is 0.
setOutlineThickness :: Shape a -> Float -> IO ()
setOutlineThickness shape thickness =
    withShape shape $ \s ->
    {# call unsafe sfShape_setOutlineThickness #} s (cFloatConv thickness)

--FIXME: Get value from IORef ?
-- |Get the source `Texture' of the `Shape'.
getTexture :: Shape a -> IO (Maybe Texture)
getTexture shape =
    withShape shape $ \s ->
    {# call unsafe sfShape_getTexture #} s >>= \res ->
    ptrToMaybe mkConstTexture res

-- |Get the sub-rectangle of the `Texture' displayed by the `Shape'.
getTextureRect :: Shape a -> IO (Rect Int)
getTextureRect shape =
    withShape shape $ \s ->
    alloca $ \rect ->
    {# call unsafe sfShape_getTextureRect_wrapper #} s rect >>
    fromIntRect <$> peek rect

-- |Get the fill `Color' of the `Shape'.
getFillColor :: Shape a -> IO Color
getFillColor shape =
    withShape shape $ \s ->
    alloca $ \color ->
    {# call unsafe sfShape_getFillColor_wrapper #} s color >>
    peek color

-- |Get the outline `Color' of the `Shape'.
getOutlineColor :: Shape a -> IO Color
getOutlineColor shape =
    withShape shape $ \s ->
    alloca $ \color ->
    {# call unsafe sfShape_getOutlineColor_wrapper #} s color >>
    peek color

-- |Get the outline thickness of the `Shape'.
getOutlineThickness :: Shape a -> IO Float
getOutlineThickness shape =
    withShape shape $ \s ->
    {# call unsafe sfShape_getOutlineThickness #} s >>= \res ->
    return $ cFloatConv res

--FIXME: Remove unsafe ?
-- |Get the number of points of the `Shape'.
getPointCount :: Shape a -> IO Word
getPointCount shape =
    withShape shape $ \s ->
    {# call unsafe sfShape_getPointCount #} s >>= \res ->
    --{# call sfShape_getPointCount #} s >>= \res ->
    return $ cIntConv res

--FIXME: Remove unsafe ?
-- |Get a point of the `Shape'.
getPoint :: Shape a -> Word -> IO (Vector2D Float)
getPoint shape index =
    withShape shape $ \s ->
    alloca $ \point ->
    {# call unsafe sfShape_getPoint_wrapper #} s (cIntConv index) point >>
    --{# call sfShape_getPoint_wrapper #} s (cIntConv index) point >>
    fromVector2F <$> peek point

-- |Get the local bounding rectangle of the `Shape'.
getLocalBounds :: Shape a -> IO (Rect Float)
getLocalBounds shape =
    withShape shape $ \s ->
    alloca $ \rect ->
    {# call unsafe sfShape_getLocalBounds_wrapper #} s rect >>
    fromFloatRect <$> peek rect

-- |Get the global bounding rectangle of the `Shape'.
getGlobalBounds :: Shape a -> IO (Rect Float)
getGlobalBounds shape =
    withShape shape $ \s ->
    alloca $ \rect ->
    {# call unsafe sfShape_getGlobalBounds_wrapper #} s rect >>
    fromFloatRect <$> peek rect

--FIXME: Remove unsafe ?
-- |Recompute the internal geometry of a `Shape'.
update :: Shape a -> IO ()
update shape =
    withShape shape $ \s ->
    --{# call unsafe sfShape_update #} s
    {# call sfShape_update #} s

