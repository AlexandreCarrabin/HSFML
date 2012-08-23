{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module SFML.Graphics.Internal.RenderWindow where


import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Word (Word)
import Foreign (FinalizerPtr, Ptr, alloca, newForeignPtr, peek, with)
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar, CFloat, CInt, CUChar, CUInt, CULong)

{# import SFML.Graphics.Internal.Image #} (mkImage)
{# import SFML.Graphics.Internal.PrimitiveType #} (PrimitiveType)
{# import SFML.Graphics.Internal.Rect #} (Rect, fromIntRect, IntRectPtr)
{# import SFML.Graphics.Internal.RenderStates #} (RenderStates, RenderStatesPtr)
{# import SFML.Graphics.Internal.Color #} (Color, ColorPtr)
{# import SFML.Graphics.Internal.Types #} (Circle, CirclePtr, withCircle , Convex, ConvexPtr, withConvex, Rectangle, RectanglePtr, withRectangle, RenderWindowPtr, Shape, ShapePtr, withShape, Sprite, SpritePtr, withSprite, Text, TextPtr, withText, VertexArray, VertexArrayPtr, withVertexArray, RenderWindow(..), Image, ViewPtr, withRenderWindow, ImagePtr, CSFML_RenderWindow, View, withView)
{# import SFML.Graphics.Internal.Vertex #} (Vertex, VertexPtr)
{# import SFML.Graphics.Internal.View #} (mkView)

{# import SFML.System.Internal.Vector2 #} (Vector2D, Vector2D, Vector2D(..), Vector2IPtr, Vector2FPtr, Vector2UPtr, fromVector2F, fromVector2I, fromVector2U, toVector2I, toVector2U)

{# import SFML.Window.Internal.Event #} (Event, EventPtr)
{# import SFML.Window.Internal.VideoMode #} (VideoMode, VideoModePtr)
{# import SFML.Window.Internal.Window #} (ContextSettings, ContextSettingsPtr)
{# import SFML.Window.Internal.Window #} (Style)
{# import SFML.Window.Internal.WindowHandle #} (WindowHandle)

import SFML.Utility.Foreign (cFloatConv, cIntConv, cIntFromBool , cIntFromEnum, cIntFromEnumList, cIntToBool, withByteString, withObjectList)

#include <SFML/Graphics/RenderWindow.h>
#include <SFML/Graphics/RenderWindowWrapper.h>


-- |Construct a new `RenderWindow'.
createRenderWindow :: VideoMode -> String -> [Style] -> ContextSettings -> IO RenderWindow
createRenderWindow mode title styles context =
    with mode $ \m ->
    withCString title $ \t ->
    with context $ \c ->
    {# call unsafe sfRenderWindow_create_wrapper #} m t (cIntFromEnumList styles) c >>= \res ->
    mkRenderWindow res

-- |Construct a new `RenderWindow' from an existing `WindowHandle'.
createRenderWindowFromHandle :: WindowHandle -> ContextSettings -> IO RenderWindow
createRenderWindowFromHandle handle settings =
    with settings $ \s ->
    {# call unsafe sfRenderWindow_createFromHandle #} (cIntConv handle) s >>= \res ->
    mkRenderWindow res

mkRenderWindow :: RenderWindowPtr -> IO RenderWindow
mkRenderWindow ptr = RenderWindow <$> newForeignPtr c_destroyRenderWindow ptr

-- |Destroy a `RenderWindow'.
foreign import ccall unsafe "SFML/Graphics/RenderWindow.h &sfRenderWindow_destroy"
    c_destroyRenderWindow :: FinalizerPtr CSFML_RenderWindow

-- |Close a `RenderWindow' (but doesn't destroy the internal data).
close :: RenderWindow -> IO ()
close window =
    withRenderWindow window $ \w ->
    {# call unsafe sfRenderWindow_close #} w

-- |Tell whether or not a `RenderWindow' is opened.
isOpen :: RenderWindow -> IO Bool
isOpen window =
    withRenderWindow window $ \w ->
    {# call unsafe sfRenderWindow_isOpen #} w >>= \res ->
    return $ cIntToBool res

-- |Get the creation `ContextSettings' of a `RenderWindow'.
getSettings :: RenderWindow -> IO ContextSettings
getSettings window =
    withRenderWindow window $ \w ->
    alloca $ \settings ->
    {# call unsafe sfRenderWindow_getSettings_wrapper #} w settings >>
    peek settings

-- |Pop the `Event' on top of events stack, if any, and return it. This function is not blocking:
--if there's no pending event then it will return `Nothing'.
pollEvent :: RenderWindow -> IO (Maybe Event)
pollEvent w = 
    withRenderWindow w $ \w' ->
    alloca $ \e -> 
    {# call unsafe sfRenderWindow_pollEvent #} w' e >>= \res ->
    if cIntToBool res 
        then Just <$> peek e
        else return Nothing

-- |Wait for an `Event' and return it. This function is blocking:
--if there's no pending event then it will wait until an event is received unless an error occured, in this cas `Nothing' is returned.
waitEvent :: RenderWindow -> IO (Maybe Event)
waitEvent w = 
    withRenderWindow w $ \w' ->
    alloca $ \e -> 
    {# call unsafe sfRenderWindow_waitEvent #} w' e >>= \res ->
    if cIntToBool res 
        then Just <$> peek e
        else return Nothing

-- |Get the position of a `RenderWindow'.
getPosition :: RenderWindow -> IO (Vector2D Int)
getPosition window =
    withRenderWindow window $ \w ->
    alloca $ \pos ->
    {# call unsafe sfRenderWindow_getPosition_wrapper #} w pos >>
    fromVector2I <$> peek pos

-- |Change the position of a `RenderWindow' on screen.
setPosition :: RenderWindow -> Vector2D Int -> IO ()
setPosition window position =
    withRenderWindow window $ \w ->
    with (toVector2I position) $ \p ->
    {# call unsafe sfRenderWindow_setPosition_wrapper #} w p

-- |Get the size of the rendering region of a `RenderWindow'.
getSize :: RenderWindow -> IO (Vector2D Word)
getSize window =
    withRenderWindow window $ \w ->
    alloca $ \pos ->
    {# call unsafe sfRenderWindow_getSize_wrapper #} w pos >>
    fromVector2U <$> peek pos

-- |Change the size of the rendering region of a `RenderWindow'.
setSize :: RenderWindow -> Vector2D Word -> IO ()
setSize window position =
    withRenderWindow window $ \w ->
    with (toVector2U position) $ \p ->
    {# call unsafe sfRenderWindow_setSize_wrapper #} w p
    
-- |Change the title of a `RenderWindow'.
setTitle :: RenderWindow -> String -> IO ()
setTitle window title =
    withRenderWindow window $ \w ->
    withCString title $ \t ->
    {# call unsafe sfRenderWindow_setTitle #} w t

-- |Change the `RenderWindow''s icon. The parameter are the window, the width and height of the icon
--and the pixels array of size width * height * 4 (RGBA format)
setIcon :: RenderWindow -> Vector2D Word -> ByteString -> IO ()
setIcon window (Vector2D width height) pixels =
    withRenderWindow window $ \w ->
    withByteString pixels $ \p ->
    {# call unsafe sfRenderWindow_setIcon #} w (cIntConv width) (cIntConv height) p

-- |Show or hide a `RenderWindow'.
setVisible :: RenderWindow -> Bool -> IO ()
setVisible window visible =
    withRenderWindow window $ \w ->
    {# call unsafe sfRenderWindow_setVisible #} w (cIntFromBool visible)

-- |Show or hide the mouse cursor. (The cursor is visible by default)
setMouseCursorVisible :: RenderWindow -> Bool -> IO ()
setMouseCursorVisible window visible =
    withRenderWindow window $ \w ->
    {# call unsafe sfRenderWindow_setMouseCursorVisible #} w (cIntFromBool visible)

-- |Enable or disable vertical synchronization.
--Activating vertical synchronization will limit the number of frames displayed to the refresh rate of the monitor.
--This can avoid some visual artifacts, and limit the framerate to a good value (but not constant across different computers).
--Vertical synchronization is disabled by default.
setVerticalSyncEnabled :: RenderWindow -> Bool -> IO ()
setVerticalSyncEnabled window enabled =
    withRenderWindow window $ \w ->
    {# call unsafe sfRenderWindow_setVerticalSyncEnabled #} w (cIntFromBool enabled)

-- |Enable or disable automatic key-repeat.
--If key repeat is enabled, you will receive repeated KeyPressed events while keeping a key pressed. 
--If it is disabled, you will only get a single event when the key is pressed.
--Key repeat is enabled by default.
setKeyRepeatEnabled :: RenderWindow -> Bool -> IO ()
setKeyRepeatEnabled window enabled =
    withRenderWindow window $ \w ->
    {# call unsafe sfRenderWindow_setKeyRepeatEnabled #} w (cIntFromBool enabled)

-- |Activate or deactivate a `RenderWindow' as the current target for rendering.
setActive :: RenderWindow -> Bool -> IO Bool
setActive window active =
    withRenderWindow window $ \w ->
    {# call unsafe sfRenderWindow_setActive #} w (cIntFromBool active) >>= \res ->
    return $ cIntToBool res

-- |Display a `RenderWindow' on screen.
display :: RenderWindow -> IO ()
display window =
    withRenderWindow window $ \w ->
    {# call unsafe sfRenderWindow_display #} w

-- |Limit the framerate to a maximum fixed frequency.
setFramerateLimit :: RenderWindow -> Word -> IO ()
setFramerateLimit window limit =
    withRenderWindow window $ \w ->
    {# call unsafe sfRenderWindow_setFramerateLimit #} w (cIntConv limit)

-- |Change the joystick threshold.
--The joystick threshold is the value below which no JoystickMoved event will be generated.
--The threshold value is 0.1 by default.
setJoystickThreshold :: RenderWindow -> Float -> IO ()
setJoystickThreshold window threshold =
    withRenderWindow window $ \w ->
    {# call unsafe sfRenderWindow_setJoystickThreshold #} w (cFloatConv threshold)

-- |Get the OS-specific handle of the `Window'.
--You shouldn't need to use this function, unless you have very specific stuff to implement that SFML doesn't support,
--or implement a temporary workaround until a bug is fixed.
getSystemHandle :: RenderWindow -> IO WindowHandle
getSystemHandle window =
    withRenderWindow window $ \w ->
    {# call unsafe sfRenderWindow_getSystemHandle #} w >>= \res ->
    return $ cIntConv res

-- |Clear the `RenderWindow' with the given `Color'.
clear :: RenderWindow -> Color -> IO ()
clear window color =
    withRenderWindow window $ \w ->
    with color $ \c ->
    {# call unsafe sfRenderWindow_clear_wrapper #} w c
    
-- |Change the current active `View' of the `RenderWindow'.
setView :: RenderWindow -> View -> IO ()
setView window view =
    withRenderWindow window $ \w ->
    withView view $ \v ->
    {# call unsafe sfRenderWindow_setView #} w v

-- |Get the current active `View' of the `RenderWindow'.
getView :: RenderWindow -> IO View
getView window =
    withRenderWindow window $ \w ->
    {# call unsafe sfRenderWindow_getView #} w >>= \res ->
    mkView res

-- [Get the default `View' of a `RenderWindow'.
getDefaultView :: RenderWindow -> IO View
getDefaultView window =
    withRenderWindow window $ \w ->
    {# call unsafe sfRenderWindow_getDefaultView #} w >>= \res ->
    mkView res

-- |Get the viewport of a `View' applied to this `RenderWindow'.
getViewport :: RenderWindow -> View -> IO (Rect Int)
getViewport window view =
    withRenderWindow window $ \w ->
    withView view $ \v ->
    alloca $ \viewport ->
    {# call unsafe sfRenderWindow_getViewport_wrapper #} w v viewport >>
    fromIntRect <$> peek viewport

-- |Convert a point in `RenderWindow' coordinates into `View' coordinates
convertCoords :: RenderWindow -> Vector2D Int -> View -> IO (Vector2D Float)
convertCoords renderWindow inputPoint view =
    withRenderWindow renderWindow $ \rw ->
    with (toVector2I inputPoint) $ \input ->
    withView view $ \v ->
    alloca $ \out ->
    {# call unsafe sfRenderWindow_convertCoords_wrapper #} rw input v out >>
    fromVector2F <$> peek out

-- |Draw a `Sprite' to the `RenderWindow'.
drawSprite :: RenderWindow -> Sprite -> RenderStates -> IO ()
drawSprite renderWindow sprite states =
    withRenderWindow renderWindow $ \rw ->
    withSprite sprite $ \s ->
    with states $ \st ->
    {# call unsafe sfRenderWindow_drawSprite #} rw s st

-- |Draw a `Text' to the `RenderWindow'.
drawText :: RenderWindow -> Text -> RenderStates -> IO ()
drawText renderWindow text states =
    withRenderWindow renderWindow $ \rw ->
    withText text $ \t ->
    with states $ \st ->
    {# call unsafe sfRenderWindow_drawText #} rw t st

-- |Draw a `Shape' to the `RenderWindow'.
drawShape :: RenderWindow -> Shape a -> RenderStates -> IO ()
drawShape renderWindow shape states =
    withRenderWindow renderWindow $ \rw ->
    withShape shape $ \s ->
    with states $ \st ->
    {# call unsafe sfRenderWindow_drawShape #} rw s st

-- |Draw a `Circle' to the `RenderWindow'.
drawCircle :: RenderWindow -> Circle -> RenderStates -> IO ()
drawCircle renderWindow circle states =
    withRenderWindow renderWindow $ \rw ->
    withCircle circle $ \c ->
    with states $ \st ->
    {# call unsafe sfRenderWindow_drawCircleShape #} rw c st

-- |Draw a `Convex' to the `RenderWindow'.
drawConvex :: RenderWindow -> Convex -> RenderStates -> IO ()
drawConvex renderWindow convex states =
    withRenderWindow renderWindow $ \rw ->
    withConvex convex $ \c ->
    with states $ \st ->
    {# call unsafe sfRenderWindow_drawConvexShape #} rw c st

-- |Draw a `Rectangle' to the `RenderWindow'.
drawRectangle :: RenderWindow -> Rectangle -> RenderStates -> IO ()
drawRectangle renderWindow rectangle states =
    withRenderWindow renderWindow $ \rw ->
    withRectangle rectangle $ \r ->
    with states $ \st ->
    {# call unsafe sfRenderWindow_drawRectangleShape #} rw r st

-- |Draw a `VertexArray' to the `RenderWindow'.
drawVertexArray :: RenderWindow -> VertexArray -> RenderStates -> IO ()
drawVertexArray renderWindow array states =
    withRenderWindow renderWindow $ \rw ->
    withVertexArray array $ \a ->
    with states $ \st ->
    {# call unsafe sfRenderWindow_drawVertexArray #} rw a st

-- |Draw primitives defined by an array of vertices to a `RenderWindow'.
drawPrimitives :: RenderWindow -> [Vertex] -> PrimitiveType -> RenderStates -> IO ()
drawPrimitives window vertices primitive states =
    withRenderWindow window $ \w ->
    withObjectList vertices $ \(v, size) ->
    with states $ \st ->
    {# call unsafe  sfRenderWindow_drawPrimitives #} w v size (cIntFromEnum primitive) st

-- |Save the current OpenGL render states and matrices.
pushGLStates :: RenderWindow -> IO ()
pushGLStates window =
    withRenderWindow window $ \w ->
    {# call unsafe sfRenderWindow_pushGLStates #} w

-- |Restore the previously saved OpenGL render states and matrices.
popGLStates :: RenderWindow -> IO ()
popGLStates window =
    withRenderWindow window $ \w ->
    {# call unsafe sfRenderWindow_popGLStates #} w

-- |Reset the internal OpenGL states so that the target is ready for drawing.
resetGLStates :: RenderWindow -> IO ()
resetGLStates window =
    withRenderWindow window $ \w ->
    {# call unsafe sfRenderWindow_resetGLStates #} w

-- |Copy the current contents of a `RenderWindow' to an `Image'.
--
--This is a slow operation, whose main purpose is to make screenshots of the application. If you want to update an
--image with the contents of the window and then use it for drawing, you should rather use a `Texture' and its
--`updateFromRenderWinfow' function.
capture :: RenderWindow -> IO Image
capture window =
    withRenderWindow window $ \w ->
    {# call unsafe sfRenderWindow_capture #} w >>= \res ->
    mkImage res

