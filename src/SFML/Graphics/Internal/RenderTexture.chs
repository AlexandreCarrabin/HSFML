{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.RenderTexture where


import Control.Applicative ((<$>))
import Data.Word (Word)
import Foreign (FinalizerPtr, alloca, newForeignPtr, peek, with)
import Foreign.C.Types (CInt, CUInt)

{# import SFML.Graphics.Internal.PrimitiveType #} (PrimitiveType)
{# import SFML.Graphics.Internal.Rect #} (Rect, fromIntRect, IntRectPtr)
{# import SFML.Graphics.Internal.RenderStates #} (RenderStates, RenderStatesPtr)
{# import SFML.Graphics.Internal.Texture #} (mkConstTexture)
{# import SFML.Graphics.Internal.Color #} (Color, ColorPtr)
{# import SFML.Graphics.Internal.Types #} (CSFML_RenderTexture, RenderTexture(..), RenderTexturePtr, withRenderTexture, Circle, CirclePtr, withCircle, Convex, ConvexPtr, withConvex, Rectangle, RectanglePtr, withRectangle, Shape, ShapePtr, withShape, Sprite, SpritePtr, withSprite, Text, TextPtr, withText, Texture, TexturePtr, View, ViewPtr, withView , VertexArray, VertexArrayPtr, withVertexArray)
{# import SFML.Graphics.Internal.Vertex #} (Vertex, VertexPtr)
{# import SFML.Graphics.Internal.View #} (mkView)

{# import SFML.System.Internal.Vector2 #} (Vector2D(..), Vector2FPtr, Vector2IPtr, Vector2UPtr, fromVector2F, toVector2I, fromVector2U)

import SFML.Utility.Foreign (cIntConv, cIntFromBool, cIntToBool, cIntFromEnum, withObjectList)

#include <SFML/Graphics/RenderTexture.h>
#include <SFML/Graphics/RenderTextureWrapper.h>


-- |Construct a new `RenderTexture' of given width and height with an optionnal depth-buffer (useful only for 3D OpenGL).
createRenderTexture :: Vector2D Word -> Bool -> IO RenderTexture
createRenderTexture (Vector2D width height) depthBuffer =
    {# call unsafe sfRenderTexture_create #} (cIntConv width) (cIntConv height) (cIntFromBool depthBuffer) >>= \res ->
    mkRenderTexture res

mkRenderTexture :: RenderTexturePtr -> IO RenderTexture
mkRenderTexture ptr = RenderTexture <$> newForeignPtr c_destroyRenderTexture ptr

-- |Destroy a `RenderTexture'.
foreign import ccall unsafe "SFML/Graphics/RenderTexture.h &sfRenderTexture_destroy"
    c_destroyRenderTexture :: FinalizerPtr CSFML_RenderTexture

-- |Get the size of the rendering region of a `RenderTexture'.
getSize :: RenderTexture -> IO (Vector2D Word)
getSize renderTexture =
    withRenderTexture renderTexture $ \rt ->
    alloca $ \size ->
    {# call unsafe sfRenderTexture_getSize_wrapper #} rt size >>
    fromVector2U <$> peek size

-- |Activate or deactivate a `RenderTexture' as the current target for rendering.
setActive :: RenderTexture -> Bool -> IO Bool
setActive renderTexture active =
    withRenderTexture renderTexture $ \rt ->
    {# call unsafe sfRenderTexture_setActive #} rt (cIntFromBool active) >>= \res ->
    return $ cIntToBool res

-- |Update the contents of the `RenderTexture'.
display :: RenderTexture -> IO ()
display renderTexture =
    withRenderTexture renderTexture $ \rt ->
    {# call unsafe sfRenderTexture_display #} rt

-- |Clear the `RenderTexture' with the given `Color'.
clear :: RenderTexture -> Color -> IO ()
clear renderTexture color =
    withRenderTexture renderTexture $ \rt ->
    with color $ \c ->
    {# call unsafe sfRenderTexture_clear_wrapper #} rt c

-- |Change the current active `View' of the `RenderTexture'.
setView :: RenderTexture -> View -> IO ()
setView renderTexture view =
    withRenderTexture renderTexture $ \rt ->
    withView view $ \v ->
    {# call unsafe sfRenderTexture_setView #} rt v

-- |Get the current active `View' of the `RenderTexture'.
getView :: RenderTexture -> IO View
getView renderTexture =
    withRenderTexture renderTexture $ \rt ->
    {# call unsafe sfRenderTexture_getView #} rt >>= \res ->
    mkView res

-- [Get the default `View' of a `RenderTexture'.
getDefaultView :: RenderTexture -> IO View
getDefaultView renderTexture =
    withRenderTexture renderTexture $ \rt ->
    {# call unsafe sfRenderTexture_getDefaultView #} rt >>= \res ->
    mkView res

-- |Get the viewport of a `View' applied to this `RenderTexture'.
getViewPort :: RenderTexture -> View -> IO (Rect Int)
getViewPort renderTexture view =
    withRenderTexture renderTexture $ \rt ->
    withView view $ \v ->
    alloca $ \viewport ->
    {# call unsafe sfRenderTexture_getViewport_wrapper #} rt v viewport >>
    fromIntRect <$> peek viewport

-- |Convert a point in `RenderTexture' coordinates into `View' coordinates
convertCoords :: RenderTexture -> Vector2D Int -> View -> IO (Vector2D Float)
convertCoords renderTexture inputPoint view =
    withRenderTexture renderTexture $ \rt ->
    with (toVector2I inputPoint) $ \input ->
    withView view $ \v ->
    alloca $ \out ->
    {# call unsafe sfRenderTexture_convertCoords_wrapper #} rt input v out >>
    fromVector2F <$> peek out

-- |Draw a `Sprite' to the `RenderTexture'.
drawSprite :: RenderTexture -> Sprite -> RenderStates -> IO ()
drawSprite renderTexture sprite states =
    withRenderTexture renderTexture $ \rt ->
    withSprite sprite $ \s ->
    with states $ \st ->
    {# call unsafe sfRenderTexture_drawSprite #} rt s st

-- |Draw a `Text' to the `RenderTexture'.
drawText :: RenderTexture -> Text -> RenderStates -> IO ()
drawText renderTexture text states =
    withRenderTexture renderTexture $ \rt ->
    withText text $ \t ->
    with states $ \st ->
    {# call unsafe sfRenderTexture_drawText #} rt t st

-- |Draw a `Shape' to the `RenderTexture'.
drawShape :: RenderTexture -> Shape a -> RenderStates -> IO ()
drawShape renderTexture shape states =
    withRenderTexture renderTexture $ \rt ->
    withShape shape $ \s ->
    with states $ \st ->
    {# call unsafe sfRenderTexture_drawShape #} rt s st

-- |Draw a `Circle' to the `RenderTexture'.
drawCircle :: RenderTexture -> Circle -> RenderStates -> IO ()
drawCircle renderTexture circle states =
    withRenderTexture renderTexture $ \rt ->
    withCircle circle $ \c ->
    with states $ \st ->
    {# call unsafe sfRenderTexture_drawCircleShape #} rt c st

-- |Draw a `Convex' to the `RenderTexture'.
drawConvex :: RenderTexture -> Convex -> RenderStates -> IO ()
drawConvex renderTexture convex states =
    withRenderTexture renderTexture $ \rt ->
    withConvex convex $ \c ->
    with states $ \st ->
    {# call unsafe sfRenderTexture_drawConvexShape #} rt c st

-- |Draw a `Rectangle' to the `RenderTexture'.
drawRectangle :: RenderTexture -> Rectangle -> RenderStates -> IO ()
drawRectangle renderTexture rectangle states =
    withRenderTexture renderTexture $ \rt ->
    withRectangle rectangle $ \r ->
    with states $ \st ->
    {# call unsafe sfRenderTexture_drawRectangleShape #} rt r st

-- |Draw a `VertexArray' to the `RenderTexture'.
drawVertexArray :: RenderTexture -> VertexArray -> RenderStates -> IO ()
drawVertexArray renderTexture array states =
    withRenderTexture renderTexture $ \rt ->
    withVertexArray array $ \a ->
    with states $ \st ->
    {# call unsafe sfRenderTexture_drawVertexArray #} rt a st

-- |Draw primitives defined by an array of vertices to a `RenderTexture'.
drawPrimitives :: RenderTexture -> [Vertex] -> PrimitiveType -> RenderStates -> IO ()
drawPrimitives renderTexture vertices primitive states =
    withRenderTexture renderTexture $ \rt ->
    withObjectList vertices $ \(v, size) ->
    with states $ \st ->
    {# call unsafe sfRenderTexture_drawPrimitives #} rt v size (cIntFromEnum primitive) st

-- |Save the current OpenGL render states and matrices.
pushGLStates :: RenderTexture -> IO ()
pushGLStates renderTexture =
    withRenderTexture renderTexture $ \rt ->
    {# call unsafe sfRenderTexture_pushGLStates #} rt

-- |Restore the previously saved OpenGL render states and matrices.
popGLStates :: RenderTexture -> IO ()
popGLStates renderTexture =
    withRenderTexture renderTexture $ \rt ->
    {# call unsafe sfRenderTexture_popGLStates #} rt

-- |Reset the internal OpenGL states so that the target is ready for drawing.
resetGLStates :: RenderTexture -> IO ()
resetGLStates renderTexture =
    withRenderTexture renderTexture $ \rt ->
    {# call unsafe sfRenderTexture_resetGLStates #} rt

-- |Get the target `Texture' of a `RenderTexture'.
getTexture :: RenderTexture -> IO Texture
getTexture renderTexture =
    withRenderTexture renderTexture $ \rt ->
    {# call unsafe sfRenderTexture_getTexture #} rt >>= \res ->
    mkConstTexture res

-- |Enable or disable the smooth filter on a `RenderTexture'.
setSmooth :: RenderTexture -> Bool -> IO ()
setSmooth renderTexture smooth =
    withRenderTexture renderTexture $ \rt ->
    {# call unsafe sfRenderTexture_setSmooth #} rt (cIntFromBool smooth)

-- |Tell whether the smooth filter is enabled or not for a `RenderTexture'.
isSmooth :: RenderTexture -> IO Bool
isSmooth renderTexture =
    withRenderTexture renderTexture $ \rt ->
    {# call unsafe sfRenderTexture_isSmooth #} rt >>= \res ->
    return $ cIntToBool res

