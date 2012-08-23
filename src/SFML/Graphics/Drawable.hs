module SFML.Graphics.Drawable
    ( RenderTarget
    , Drawable(..)
    ) where


import Foreign (Ptr , castPtr, with)

import SFML.Graphics.Internal.Types (RenderTexture, withRenderTexture, RenderWindow, withRenderWindow, Circle, Convex, Rectangle, Shape, Sprite, Text, VertexArray, withSprite, withText, withCircle, withConvex, withRectangle, withShape, withVertexArray)
import SFML.Graphics.Internal.RenderStates (RenderStates)
import SFML.Graphics.Internal.RenderTexture
import SFML.Graphics.Internal.RenderWindow


data RenderTargetType = RenderWindowTarget
                      | RenderTextureTarget

class RenderTarget a where
    renderType :: a -> RenderTargetType
    withRenderTarget :: a -> (Ptr () -> IO b) -> IO b

instance RenderTarget RenderTexture where
    renderType _ = RenderTextureTarget
    withRenderTarget rt act = withRenderTexture rt $ \ptr -> act (castPtr ptr)

instance RenderTarget RenderWindow where
    renderType _ = RenderWindowTarget
    withRenderTarget rw act = withRenderWindow rw $ \ptr -> act (castPtr ptr)


class Drawable a where
    -- |Draw a `Drawable' object to the `RenderTarget'. 
    draw :: RenderTarget r => r -> a -> RenderStates -> IO ()

instance Drawable Sprite where
    draw target sprite states =
        withRenderTarget target $ \ptr ->
        withSprite sprite $ \s ->
        with states $ \st ->
        case renderType target of
            RenderTextureTarget -> sfRenderTexture_drawSprite ptr s st
            RenderWindowTarget  -> sfRenderWindow_drawSprite ptr s st

instance Drawable Text where
    draw target text states =
        withRenderTarget target $ \ptr ->
        withText text $ \t ->
        with states $ \st ->
        case renderType target of
            RenderTextureTarget -> sfRenderTexture_drawText ptr t st
            RenderWindowTarget  -> sfRenderWindow_drawText ptr t st

instance Drawable (Shape a) where
    draw target shape states =
        withRenderTarget target $ \ptr ->
        withShape shape $ \s ->
        with states $ \st ->
        case renderType target of
            RenderTextureTarget -> sfRenderTexture_drawShape ptr s st
            RenderWindowTarget  -> sfRenderWindow_drawShape ptr s st

instance Drawable Circle where
    draw target circle states =
        withRenderTarget target $ \ptr ->
        withCircle circle $ \c ->
        with states $ \st ->
        case renderType target of
            RenderTextureTarget -> sfRenderTexture_drawCircleShape ptr c st
            RenderWindowTarget  -> sfRenderWindow_drawCircleShape ptr c st

instance Drawable Convex where
    draw target convex states =
        withRenderTarget target $ \ptr ->
        withConvex convex $ \c ->
        with states $ \st ->
        case renderType target of
            RenderTextureTarget -> sfRenderTexture_drawConvexShape ptr c st
            RenderWindowTarget  -> sfRenderWindow_drawConvexShape ptr c st

instance Drawable Rectangle where
    draw target rectangle states =
        withRenderTarget target $ \ptr ->
        withRectangle rectangle $ \r ->
        with states $ \st ->
        case renderType target of
            RenderTextureTarget -> sfRenderTexture_drawRectangleShape ptr r st
            RenderWindowTarget  -> sfRenderWindow_drawRectangleShape ptr r st

instance Drawable VertexArray where
    draw target array states =
        withRenderTarget target $ \ptr ->
        withVertexArray array $ \a ->
        with states $ \st ->
        case renderType target of
            RenderTextureTarget -> sfRenderTexture_drawVertexArray ptr a st
            RenderWindowTarget  -> sfRenderWindow_drawVertexArray ptr a st

